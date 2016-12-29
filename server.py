#!/usr/bin/env python
import os, socket, getpass
import sys, traceback
import time
import logging
import tempfile
import parsegit

from watchdog.observers import Observer
from watchdog.events import LoggingEventHandler
from watchdog.events import FileSystemEventHandler

class MyHandler(FileSystemEventHandler):
    def __init__(self, repo):
        self.repo = repo

    def on_modified(self, event):
        print "Got it!"
        print event
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
            repos[self.path].stat = parsegit.parse(self.path)
            self.needsUpdate = False
        return repos[self.path].stat

repos = {}

def register(path):
    try:
        return repos[path].status()
    except KeyError:
        print "registering " + path
        r = Repo()
        repos[path] = r
        r.handler = MyHandler(r)
        r.path = path
        observer.schedule(r.handler, path, recursive=True)

        return r.status()

    except Exception as e:
        traceback.print_exc()
        pass

    return ""

#### watchdog
observer = Observer()
observer.start()

sockfile = tempfile.gettempdir() + "/gitstatus-" + getpass.getuser()

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
            stat = register(path)
            conn.send("status " + stat)

