#!/usr/bin/env python
from __future__ import print_function

# change this symbol to whatever you prefer
prehash = ':'

from subprocess import Popen, PIPE, check_output, STDOUT
from threading import Thread
import sys, os

if len(sys.argv) > 1:
    os.chdir(sys.argv[1])

# from http://stackoverflow.com/a/4825933/1562506
class Command(object):
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

gitsym = Command(['git', 'symbolic-ref', 'HEAD'])
branch, error = gitsym.run(0.01)

error_string = error.decode('utf-8')


if 'fatal: Not a git repository' in error_string or len(branch) == 0:
	sys.exit(0)

branch = branch.decode("utf-8").strip()[11:]

gitstatus = Command(['git','status','--porcelain',]).run()
err_string = gitstatus[1].decode('utf-8')
if 'fatal' in err_string:
	sys.exit(0)

lines = gitstatus[0].decode("utf-8").splitlines()

nb_changed = len([0 for line in lines if line.startswith(" M")])
nb_untracked = len([0 for line in lines if line.startswith("??")])
nb_staged = len([0 for line in lines if line.startswith("M ") or line.startswith("A ")])
nb_conflicts = len([0 for line in lines if line.startswith("UU")])

staged = str(nb_staged)
conflicts = str(nb_conflicts)
changed = str(nb_changed)
untracked = str(nb_untracked)

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
print(out, end='')

