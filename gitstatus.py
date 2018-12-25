#!/usr/bin/env python3
from __future__ import print_function
import sys, os

import socket, getpass
import traceback
import time
import logging
import tempfile

from subprocess import Popen, PIPE, check_output, STDOUT
from threading import Thread


# change this symbol to whatever you prefer
hash_prefix = ':'
enable_watchdog = False
maxwatch = 1000
maxwatch_repo = 400
update_timeout = 0.5
buffer_size = 64

if enable_watchdog:
    from watchdog.observers import Observer
    from watchdog.events import LoggingEventHandler
    from watchdog.events import FileSystemEventHandler


    class MyHandler(FileSystemEventHandler):
        def __init__(self, repo):
            self.repo = repo

        def on_modified(self, event):
            print("event " + str(event))
            print(event.src_path)
            print("invalidate " + self.repo.path)
            self.repo.needs_update = True

# global variables
watch_count = 0
sockfile = tempfile.gettempdir() + "/gitstatus-" + getpass.getuser()


# print on stderr
def eprint(*args, **kwargs):
    print(*args, file=sys.stderr, **kwargs)


def count_dirs(path, max=-1, accu=0):
    accu += 1
    for f in os.listdir(path):
        child = os.path.join(path, f)
        if os.path.isdir(child):
            accu = count_dirs(child, max, accu)
        if max > 0 and accu > max:
            return accu
    return accu

# from http://stackoverflow.com/a/4825933/1562506
class Command(object):
    """execute a command, with a default timeout"""

    def __init__(self, cmd):
        self.cmd = cmd
        self.process = None
        self.stdout = None
        self.stderr = None

    def run(self, timeout=0.10):
        def target():
            self.process = Popen(self.cmd, stdout=PIPE, stderr=PIPE)
            self.stdout,self.stderr = self.process.communicate()

        thread = Thread(target=target)
        thread.start()

        thread.join(timeout)
        if thread.is_alive():
            if(self.process != None):
                self.process.terminate()
            thread.join()
        return (self.stdout, self.stderr)

    def poll(self):
        return self.process.poll()


def get_git_base_dir(path):
    os.chdir(path)
    gitsym, error = Command(['git', 'rev-parse', '--show-toplevel', '--symbolic-full-name', 'HEAD']).run(0.01)
    gitsym = gitsym.decode('utf8')

    error_string = error.decode('utf-8')
    if 'fatal: Not a git repository' in error_string or len(gitsym) == 0:
        return None, None

    path = gitsym.split("\n")[0]
    branch = gitsym.split("\n")[1]
    branch = branch.strip()[11:]

    return path, branch



def git_status(path):
    eprint("get status of " + path)
    path, branch = get_git_base_dir(path)
    if(path == None):
        return ""

    gitstatus = Command(['git','status','--porcelain',]).run()
    err_string = gitstatus[1].decode('utf-8')
    if 'fatal' in err_string:
        return ""

    lines = gitstatus[0].decode("utf-8").splitlines()

    def count(lines, idx, char):
        c = 0
        for line in lines:
            if len(line) < 2: continue
            c += line[idx:idx+len(char)] == char
        return c

    staged = str(count(lines, 0, "M") + count(lines, 0, "A"))
    conflicts = str(count(lines, 0, "UU"))
    changed = str(count(lines, 1, "M"))
    untracked = str(count(lines, 0, "??"))

    ahead, behind = 0,0

    if not branch: # not on any branch
            branch = hash_prefix + Command(['git','rev-parse','--short','HEAD']).run()[0].decode("utf-8")[:-1]
    else:
            remote_name = Command(['git','config','branch.%s.remote' % branch]).run()[0].decode("utf-8").strip()
            if remote_name:
                    merge_name = Command(['git','config','branch.%s.merge' % branch]).run()[0].decode("utf-8").strip()
                    if remote_name == '.': # local
                            remote_ref = merge_name
                    else:
                            remote_ref = 'refs/remotes/%s/%s' % (remote_name, merge_name[11:])
                    revgit = Command(['git', 'rev-list', '--left-right', '%s...HEAD' % remote_ref])
                    revlist = revgit.run()[0]
                    if revgit.poll(): # fallback to local
                            revlist = Command(['git', 'rev-list', '--left-right', '%s...HEAD' % merge_name]).run()[0]
                    behead = revlist.decode("utf-8").splitlines()
                    ahead = len([x for x in behead if x[0]=='>'])
                    behind = len(behead) - ahead

    out = "status " + ' '.join([
            branch,
            str(ahead),
            str(behind),
            staged,
            conflicts,
            changed,
            untracked,
            ])
    return out



class Repo:
    def __init__(self):
        self.path = None
        self.stat = ""
        self.needs_update = True
        self.handler = None

    def __str__(self):
        return str(self.stat)

    def status(self):
        if self.needs_update or (self.handler == None and self.timestamp + update_timeout < time.time()):
            self.stat = git_status(self.path)
            self.timestamp = time.time()
            self.needs_update = False
        return self.stat



def server_is_running():
    conn = socket.socket( socket.AF_UNIX, socket.SOCK_STREAM )
    try:
        conn.connect(sockfile)
        return True
    except Exception as e:
        return False

class Server:
    def __init__(self):
        self.repos = {}
        if enable_watchdog:
            self.observer = Observer()

    def start(self):
        if server_is_running():
            return

        if enable_watchdog:
            self.observer.start()

        if os.path.exists( sockfile ):
          os.remove( sockfile )

        sock = socket.socket( socket.AF_UNIX, socket.SOCK_STREAM )
        sock.bind(sockfile)
        sock.listen(5)

        commands = {}
        commands["ping"] = lambda send, data: send(str(os.getpid()))
        def getCommand(send, data):
            path = data[4:]
            eprint(self.repos)
            stat = self.get_git_status(path)
            if stat == "": stat = "fail"
            send(stat)
        commands["get"] = getCommand

        def infoCommand(send, data):
            reply = "size: " + str(len(self.repos)) + "\n"
            reply += "content: " + str(self.repos.keys())
            send(reply)
        commands["info"] = infoCommand

        def killCommand(send, data):
            send("shutting down server")
            sys.exit(0)
        commands["kill"] = killCommand

        eprint("run server")
        while True:
          conn, addr = sock.accept()

          def send(msg):
            conn.send(msg.encode('utf8'))

          while True:
            data = conn.recv( buffer_size )
            if not data:
                break
            else:
                data = data.decode('utf8')
                eprint("received command " + data)
                cmd = data.split(" ")[0]
                if not cmd in commands:
                    send("unknown command, allowed: " + ", ".join(commands.keys()))
                else:
                    commands[cmd](send, data)
            eprint("---")
        eprint("stopping server")


    def get_git_status(self, path):
        global watch_count
        path = get_git_base_dir(path)[0]
        if path == None or not ".git" in os.listdir(path):
            eprint("only register git repos")
            return ""

        try:
            return self.repos[path].status()
        except KeyError:
            eprint("registering " + path)
            r = Repo()
            self.repos[path] = r
            r.path = path

            if enable_watchdog:
                dir_count = count_dirs(path, max=maxwatch_repo)
                eprint("number of directories: " + str(dir_count))
                if dir_count < maxwatch_repo and watch_count + dir_count < maxwatch:
                    eprint("observe file system changes")
                    r.handler = MyHandler(r)
                    try:
                        self.observer.schedule(r.handler, path, recursive=True)
                        watch_count += dir_count
                    except OSError as e:
                        traceback.print_exc()
                        r.handler = None

            return r.status()

        except Exception as e:
            traceback.print_exc()
            pass

        return ""

def start_server():
# https://code.activestate.com/recipes/278731/
    if server_is_running():
        return

    if (hasattr(os, "devnull")):
        redirect = os.devnull
    else:
        redirect = "/dev/null"

    try:
        # Store the Fork PID
        pid = os.fork()
        if pid > 0:
          return

        os.setsid()

        try:
             pid = os.fork()	# Fork a second child.
        except OSError as e:
             raise Exception("%s [%d]" % (e.strerror, e.errno))

        if pid > 0:
            os._exit(0)

        os.chdir("/")
        os.umask(0)

        # close file descriptors
        import resource
        maxfd = resource.getrlimit(resource.RLIMIT_NOFILE)[1]
        if (maxfd == resource.RLIM_INFINITY):
           maxfd = 1024

        for fd in range(0, maxfd):
           try:
              os.close(fd)
           except OSError:	# ERROR, fd wasn't open to begin with (ignored)
              pass

        # stdin to stdout and stderr
        os.open(redirect, os.O_RDWR)
        os.dup2(0, 1)
        os.dup2(0, 2)

        eprint("starting server")
        server = Server()
        server.start()
    except OSError as error:
        traceback.print_exc()
        print('Unable to fork. Error: ' + str(error))
        return


def request(cmd):
    conn = socket.socket( socket.AF_UNIX, socket.SOCK_STREAM )
    #eprint("connecting to " + sockfile)
    try:
        conn.connect(sockfile)
        #eprint("connected")
    except Exception as e:
        eprint("failed to connect to " + sockfile)
        traceback.print_exc()
        return None

    conn.send(cmd.encode('utf8'))
    return conn.recv(buffer_size).decode('utf8')


def request_status(path):
    reply = request("get " + path)

    if reply == None:
        start_server()
        return git_status(path)

    return reply

if __name__ == "__main__":
    args = sys.argv[1:]
    mode = None
    if len(args) > 0:
        mode = args[0]
        args = args[1:]

        if "server" == mode:
            server = Server()
            server.start()
            sys.exit(0)

        if "daemon" == mode:
            start_server()
            sys.exit(0)

        if "client" == mode:
            print(request(" ".join(args)))
            sys.exit(0)

    if len(args) > 0:
        path = args[0]
    else:
        path = "."
    path = os.path.abspath(path)

    if "count" == mode:
        print(count_dirs(path, max=100))

    print(request_status(path), end='')
