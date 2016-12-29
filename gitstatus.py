#!/usr/bin/env python
from __future__ import print_function
import sys, os

import socket, getpass
import traceback
import time
import logging
import tempfile
import parsegit

from subprocess import Popen, PIPE, check_output, STDOUT
from threading import Thread

from watchdog.observers import Observer
from watchdog.events import LoggingEventHandler
from watchdog.events import FileSystemEventHandler

sockfile = tempfile.gettempdir() + "/gitstatus-" + getpass.getuser()


# change this symbol to whatever you prefer
prehash = ':'

def get_path():
    """path from command line or current working directory"""
    if len(sys.argv) > 1:
        path = sys.argv[1]
    else:
        path = "."
    return os.path.abspath(path)

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
            self.process.terminate()
            thread.join()
        return (self.stdout, self.stderr)

    def poll(self):
        return self.process.poll()



def parse(path=get_path()):
    print("parse " + path)

    os.chdir(path)
    gitsym, error = Command(['git', 'rev-parse', '--show-toplevel', '--symbolic-full-name', 'HEAD']).run(0.01)

    error_string = error.decode('utf-8')
    if 'fatal: Not a git repository' in error_string or len(gitsym) == 0:
            sys.exit(0)

    path = gitsym.split("\n")[0]
    branch = gitsym.split("\n")[1]
    branch = branch.decode("utf-8").strip()[11:]

    gitstatus = Command(['git','status','--porcelain',]).run()
    err_string = gitstatus[1].decode('utf-8')
    if 'fatal' in err_string:
            sys.exit(0)

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
            branch = prehash + Command(['git','rev-parse','--short','HEAD']).run()[0].decode("utf-8")[:-1]
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

    out = ' '.join([
            branch,
            str(ahead),
            str(behind),
            staged,
            conflicts,
            changed,
            untracked,
            ])
    return out


stat = parsegit.parse()
print("status " + str(stat), end='')


class MyHandler(FileSystemEventHandler):
    def __init__(self, repo):
        self.repo = repo

    def on_modified(self, event):
        self.repo.needsUpdate = True

class Repo:
    def __init__(self):
        self.path = None
        self.stat = ""
        self.needsUpdate = True

    def __str__(self):
        return str(self.path)

    def status(self):
        if self.needsUpdate:
            self.stat = parsegit.parse(self.path)
            self.needsUpdate = False
        return self.stat

class Server:
    def __init__(self):
        self.observer = Observer()
        self.repos = {}

    def start(self):
        self.observer.start()
        if os.path.exists( sockfile ):
          os.remove( sockfile )

        server = socket.socket( socket.AF_UNIX, socket.SOCK_STREAM )
        server.bind(sockfile)
        server.listen(5)

        while True:
          conn, addr = server.accept()
          while True:
            data = conn.recv( 8096 )
            if not data:
                break
            else:
                if "done" == data:
                    break
                if "ping" == data:
                    conn.send(str(os.getpid()))
                if "get " == data[0:4]:
                    path = data[4:]
                    stat = self.register(path)
                    conn.send("status " + stat)


    def register(self, path):
        try:
            return self.repos[path].status()
        except KeyError:
            print("registering " + path)
            r = Repo()
            self.repos[path] = r
            r.handler = MyHandler(r)
            r.path = path
            self.observer.schedule(r.handler, path, recursive=True)

            return r.status()

        except Exception as e:
            traceback.print_exc()
            pass

        return ""

def startServer():
  try:
    # Store the Fork PID
    pid = os.fork()
    if pid > 0:
      return

  except OSError, error:
    print('Unable to fork. Error: %d (%s)' % (error.errno, error.strerror))
    return

  server = Server()
  server.start()



def get(path):
    try:
        conn = socket.socket( socket.AF_UNIX, socket.SOCK_STREAM )
        conn.connect(sockfile)

        conn.send("get " + path)
        return conn.recv(1024)
    except:
        startServer()
        return parsegit.parse(path)

if __name__ == "__main__":
    print(get(parsegit.get_path()), end='')
