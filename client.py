#!/usr/bin/env python
from __future__ import print_function

import os, socket, getpass
import sys
import time
import logging
import tempfile
import parsegit


sockfile = tempfile.gettempdir() + "/gitstatus-" + getpass.getuser()

def get(path):
    try:
        conn = socket.socket( socket.AF_UNIX, socket.SOCK_STREAM )
        conn.connect(sockfile)

        conn.send("get " + path)
        return conn.recv(1024)
    except:
        return parsegit.parse(path)

if __name__ == "__main__":
    print(get(parsegit.get_path()), end='')
