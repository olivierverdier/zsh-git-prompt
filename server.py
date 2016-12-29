#!/usr/bin/env python
from __future__ import print_function
import os, socket, getpass
import sys, traceback
import time
import logging
import tempfile
import parsegit

from watchdog.observers import Observer
from watchdog.events import LoggingEventHandler
from watchdog.events import FileSystemEventHandler

sockfile = tempfile.gettempdir() + "/gitstatus-" + getpass.getuser()

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
