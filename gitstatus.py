#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
Retrieve information about the git repository in CWD.

Invoked by ./zshrc.sh automatically.
"""
import os
import sys
from subprocess import Popen, PIPE

# This symbol appears before hashes when detached
SYM_PREHASH = os.environ.get('ZSH_THEME_GIT_PROMPT_HASH_PREFIX', ':')


# TODO: Delete pcmd_error, always check rcode of Popen
#       If not 0, raise ProcessError and sys.exit/return in main.
def pcmd(cmd):
    """
    Run a simple command and return the output on complete.

    Condition: Command will block until completed.
               Use Popen for more control.

    Args:
        cmd: The command to run as a list of strings.

    Returns:
        out: unicode string of stdout
    """
    out, _ = Popen(cmd, stdout=PIPE).communicate()
    out = out.decode('utf-8', errors='ignore').strip()

    return out


def pcmd_error(cmd):
    """
    Run a simple command and return the output and stderr on complete.

    Condition: Command will block until completed.
               Use Popen for more control.

    Args:
        cmd: The command to run as a list of strings.

    Returns:
        out: unicode string of stdout
        err: unicode string of stderr
    """
    out, err = Popen(cmd, stdout=PIPE, stderr=PIPE).communicate()
    out = out.decode('utf-8', errors='ignore').strip()
    err = err.decode('utf-8', errors='ignore').strip()

    return out, err


def compute_stats():
    """
    Computes and returns the following _numbers_ based on repo in the CWD.
        staged files
        conflicts
        changed
        stashed files
        untracked

    Returns:
        (# staged files, # conflicts, # changed, # stashed, # untracked)
    """
    out, err = pcmd_error(['git', 'diff', '--name-status'])
    if 'fatal' in err.lower():
        sys.exit(0)
    changed_files = [line[0] for line in out.splitlines()]

    out = pcmd(['git', 'diff', '--staged', '--name-status'])
    staged_files = [line[0] for line in out.splitlines()]
    changed = len(changed_files) - changed_files.count('U')
    staged = len(staged_files) - staged_files.count('U')

    out = pcmd(['git', 'status', '--porcelain'])
    untracked = len([0 for status in out.splitlines() if status.startswith('??')])

    conflicts = staged_files.count('U')
    stashed = len(pcmd(['git', 'stash', 'list']).splitlines())

    return staged, conflicts, changed, untracked, stashed


def compute_ahead_behind(branch):
    """
    Computes how far behind and/or ahead the current branch is from remote.

    Args:
        branch: The branch we are tracking remotely.

    Returns:
        (# commits behind, # commits ahead)
    """
    remote_name = pcmd(['git', 'config', 'branch.%s.remote' % branch])
    if remote_name:
        merge_name = pcmd(['git', 'config', 'branch.%s.merge' % branch])
    else:
        remote_name = u"origin"
        merge_name = u"refs/heads/%s" % branch

    if remote_name == '.':  # local
        remote_ref = merge_name
    else:
        remote_ref = 'refs/remotes/%s/%s' % (remote_name, merge_name[11:])

    revgit = Popen(['git', 'rev-list', '--left-right', '%s...HEAD' % remote_ref],
                   stdout=PIPE, stderr=PIPE)
    if revgit.poll():  # fallback to local
        revlist = pcmd(['git', 'rev-list', '--left-right', '%s...HEAD' % merge_name])
    else:
        revlist = revgit.communicate()[0].decode("utf-8")

    behead = revlist.splitlines()
    ahead = len([x for x in behead if x[0] == '>'])
    behind = len(behead) - ahead

    return behind, ahead


def main():
    """ Main entry point. """
    out, err = pcmd_error(['git', 'symbolic-ref', 'HEAD'])
    if 'fatal: not a git repository' in err.lower():
        sys.exit(0)
    branch = out[11:]

    remote = 0, 0
    if branch:
        remote = compute_ahead_behind(branch)
    else:
        branch = SYM_PREHASH + pcmd(['git', 'rev-parse', '--short', 'HEAD'])

    values = [str(x) for x in (branch,) + remote + compute_stats()]
    sys.stdout.write('\n'.join(values) + '\n')
    sys.stdout.flush()


if __name__ == "__main__":
    main()
