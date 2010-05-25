#!/usr/bin/env python
# -*- coding: UTF-8 -*-

from subprocess import Popen, PIPE

output = Popen(['git','status'], stdout=PIPE).communicate()[0]
lines = output.splitlines()

import re
behead_re = re.compile(r"^# Your branch is (ahead of|behind) '(.*)' by (\d+) commit")
diverge_re = re.compile(r"^# and have (\d+) and (\d+) different")
symbols = {'ahead of': '↑', 'behind': '↓'}

status = ''
staged = re.compile(r'^# Changes to be committed:$', re.MULTILINE)
changed = re.compile(r'^# Changed but not updated:$', re.MULTILINE)
untracked = re.compile(r'^# Untracked files:$', re.MULTILINE)

if staged.search(output):
	nb = len(Popen(['git','diff','--staged','--numstat'], stdout=PIPE).communicate()[0].splitlines())
	status += '♦%d' % nb
if changed.search(output):
	nb = len(Popen(['git','diff','--numstat'], stdout=PIPE).communicate()[0].splitlines())
	status += '∘%d' % nb
if untracked.search(output):
	status += '.'
if status == '':
	status = '⚡'

bline = lines[0]
if bline.find('Not currently on any branch') != -1:
	branch = ':'+ Popen(['git','rev-parse','--short','HEAD'], stdout=PIPE).communicate()[0][:-1]
else:
	branch = bline.split(' ')[3]
	bstatusline = lines[1]
	match = behead_re.match(bstatusline)
	if match:
		branch += symbols[match.groups()[0]]
		branch += match.groups()[2]
	elif lines[2:]:
		div_match = diverge_re.match(lines[2])
	 	if div_match:
			branch += "|{1}↕{0}".format(*div_match.groups())
print '(%s%s)' % (branch,status)

