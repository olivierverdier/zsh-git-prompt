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


class ProcessError(Exception):
    """
    There was a problem running the command.
    """
    pass


def run_cmd(cmd):
    """
    Run a simple command and return the output on complete.
    Command will block until completed. Use Popen for more control.

    Args:
        cmd: The command to run as a list of strings.

    Returns:
        out: unicode string of stdout

    Raises:
        ProcessError - The returncode was not 0.
    """
    with open(os.devnull, 'w') as devnull:
        proc = Popen(cmd, stdout=PIPE, stderr=devnull)

    out, _ = proc.communicate()
    if proc.returncode:
        raise ProcessError(proc.returncode, cmd)

    return out.decode('utf-8', errors='ignore').strip()


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
    out = run_cmd(['git', 'diff', '--name-status'])
    changed_files = [line[0] for line in out.splitlines()]

    out = run_cmd(['git', 'diff', '--staged', '--name-status'])
    staged_files = [line[0] for line in out.splitlines()]
    changed = len(changed_files) - changed_files.count('U')
    staged = len(staged_files) - staged_files.count('U')

    out = run_cmd(['git', 'status', '--porcelain'])
    untracked = len([0 for status in out.splitlines() if status.startswith('??')])

    conflicts = staged_files.count('U')
    stashed = len(run_cmd(['git', 'stash', 'list']).splitlines())

    return staged, conflicts, changed, untracked, stashed


def compute_ahead_behind(branch):
    """
    Computes how far behind and/or ahead the current branch is from remote.

    Args:
        branch: The branch we are tracking remotely.

    Returns:
        (# commits behind, # commits ahead)
    """
    try:
        remote_name = run_cmd(['git', 'config', 'branch.%s.remote' % branch])
        merge_name = run_cmd(['git', 'config', 'branch.%s.merge' % branch])
    except ProcessError:
        remote_name = "origin"
        merge_name = "refs/heads/%s" % branch

    if remote_name == '.':  # local
        remote_ref = merge_name
    else:
        remote_ref = 'refs/remotes/%s/%s' % (remote_name, merge_name[11:])

    revgit = Popen(['git', 'rev-list', '--left-right', '%s...HEAD' % remote_ref],
                   stdout=PIPE, stderr=PIPE)
    if revgit.poll():  # fallback to local
        revlist = run_cmd(['git', 'rev-list', '--left-right', '%s...HEAD' % merge_name])
    else:
        revlist = revgit.communicate()[0].decode("utf-8")

    behead = revlist.splitlines()
    ahead = len([x for x in behead if x[0] == '>'])
    behind = len(behead) - ahead

    return behind, ahead


def main():
    """ Main entry point. """
    try:
        branch = run_cmd(['git', 'symbolic-ref', 'HEAD'])[11:]
        remote = 0, 0

        if branch:
            remote = compute_ahead_behind(branch)
        else:
            branch = SYM_PREHASH + run_cmd(['git', 'rev-parse', '--short', 'HEAD'])

        values = [str(x) for x in (branch,) + remote + compute_stats()]
        sys.stdout.write(' '.join(values))
        sys.stdout.flush()
    except ProcessError:
        pass


if __name__ == "__main__":
    main()
