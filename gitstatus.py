#!/usr/bin/env python
# -*- coding: UTF-8 -*-

from subprocess import Popen, PIPE

output = Popen(['git','status'], stdout=PIPE).communicate()[0]
lines = output.splitlines()

import re
branch_re = re.compile(r"^\# Your branch is (ahead|behind of) of '(.*)' by (\d+) commit")
symbols = {'ahead': '↑', 'diverged': '↕', 'behind': '↓'}

bline = lines[0]
if bline.find('Not currently on any branch') != -1:
	branch = '∅'
else:
	branch = bline.split(' ')[3]
## 	if branch == 'master':
## 		branch = '✦'
	bstatusline = lines[1]
	match = branch_re.match(bstatusline)
	if match:
		branch += symbols[match.groups()[0]]
		branch += match.groups()[2]
	if bstatusline.find('nothing to commit (working directory clean)') != -1:
		branch += '⚡'
	if bstatusline.find('diverged') != -1:
		branch += symbols['diverged']
print '(%s)' % branch

