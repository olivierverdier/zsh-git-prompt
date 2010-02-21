#!/usr/bin/env python
# -*- coding: UTF-8 -*-

from subprocess import Popen, PIPE

output = Popen(['git','status'], stdout=PIPE).communicate()[0]
lines = output.splitlines()

import re
behead_re = re.compile(r"^# Your branch is (ahead of|behind) '(.*)' by (\d+) commit")
diverge_re = re.compile(r"^# Your branch and '.*' have diverged")
symbols = {'ahead of': '↑', 'diverged': '↕', 'behind': '↓'}

bline = lines[0]
if bline.find('Not currently on any branch') != -1:
	branch = '∅'
else:
	branch = bline.split(' ')[3]
## 	if branch == 'master':
## 		branch = '♦'
	bstatusline = lines[1]
	match = behead_re.match(bstatusline)
	if match:
		branch += symbols[match.groups()[0]]
		branch += match.groups()[2]
	elif bstatusline.find('nothing to commit (working directory clean)') != -1:
		branch += '⚡'
	elif diverge_re.match(bstatusline):
		branch += symbols['diverged']
print '(%s)' % branch

