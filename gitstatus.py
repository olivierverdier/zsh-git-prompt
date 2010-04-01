#!/usr/bin/env python
# -*- coding: UTF-8 -*-

from subprocess import Popen, PIPE

output = Popen(['git','status'], stdout=PIPE).communicate()[0]
lines = output.splitlines()

import re
behead_re = re.compile(r"^# Your branch is (ahead of|behind) '(.*)' by (\d+) commit")
diverge_re = re.compile(r"^# and have (\d) and (\d) different")
symbols = {'ahead of': '↑', 'behind': '↓'}

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
	else:
		div_match = diverge_re.match(lines[2])
	 	if div_match:
			branch += "|{1}↕{0}".format(*div_match.groups())
print '(%s)' % branch

