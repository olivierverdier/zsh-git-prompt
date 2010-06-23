#!/usr/bin/env python
# -*- coding: UTF-8 -*-

# change those symbols to whatever you prefer
symbols = {'ahead of': '↑', 'behind': '↓', 'staged':'♦', 'changed':'‣', 'untracked':'…', 'clean':'⚡', 'unmerged':'≠', 'sha1':':'}

from subprocess import Popen, PIPE

branch,error = Popen(['git', 'symbolic-ref', 'HEAD'], stdout=PIPE, stderr=PIPE).communicate()

if error.find('fatal: Not a git repository') != -1:
	import sys
	sys.exit(0)

branch = branch[11:-1]

status = ''

changed = [namestat[0] for namestat in Popen(['git','diff','--name-status'], stdout=PIPE).communicate()[0].splitlines()]
staged = [namestat[0] for namestat in Popen(['git','diff', '--staged','--name-status'], stdout=PIPE).communicate()[0].splitlines()]
nb_changed = len(changed) - changed.count('U')
nb_U = staged.count('U')
nb_staged = len(staged) - nb_U
if nb_staged:
	status += '%s%s' % (symbols['staged'], nb_staged)
if nb_U:
	status += '%s%s' % (symbols['unmerged'], nb_U)
if nb_changed:
	status += '%s%s' % (symbols['changed'], nb_changed)
nb = len(Popen(['git','ls-files','--others','--exclude-standard'],stdout=PIPE).communicate()[0].splitlines())
if nb:
	status += symbols['untracked']
if status == '':
	status = symbols['clean']

remote = ''

if not branch: # not on any branch
	branch = symbols['sha1']+ Popen(['git','rev-parse','--short','HEAD'], stdout=PIPE).communicate()[0][:-1]
else:
	remote_name = Popen(['git','config','branch.%s.remote' % branch], stdout=PIPE).communicate()[0].strip()
	if remote_name:
		merge_name = Popen(['git','config','branch.%s.merge' % branch], stdout=PIPE).communicate()[0].strip()
		if remote_name == '.': # local
			remote_ref = merge_name
		else:
			remote_ref = 'refs/remotes/%s/%s' % (remote_name, merge_name[11:])
		revlist,err = Popen(['git', 'rev-list', '--left-right', '%s...HEAD' % remote_ref],stdout=PIPE,stderr=PIPE).communicate()
		if err.find('fatal:') != -1: # fallback to local
			revlist,err = Popen(['git', 'rev-list', '--left-right', '%s...HEAD' % merge_name],stdout=PIPE,stderr=PIPE).communicate()
		behead = revlist.splitlines()
		ahead = len([x for x in behead if x[0]=='>'])
		behind = len(behead) - ahead
		if behind:
			remote += '%s%s' % (symbols['behind'], behind)
		if ahead:
			remote += '%s%s' % (symbols['ahead of'], ahead)
	
print '\n'.join([branch,remote,status])

