#!/usr/bin/env python
from __future__ import print_function
import sys, os

import parsegit


stat = parsegit.parse()
print("status " + str(stat), end='')


