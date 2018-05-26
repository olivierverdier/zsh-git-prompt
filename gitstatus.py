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

    conflicts = staged_files.count('U')
    staged = len(staged_files) - conflicts
    changed = len(changed_files) - changed_files.count('U')

    out = run_cmd(['git', 'status', '--porcelain'])
    untracked = len([0 for status in out.splitlines() if status.startswith('??')])
    stashed = len(run_cmd(['git', 'stash', 'list']).splitlines())

    return staged, conflicts, changed, untracked, stashed


def compute_ahead_behind(branch):
    """
    Computes how far behind and/or ahead the current branch is from remote.

    Args:
        branch: The branch we are tracking remotely.

    Returns:
        (# commits behind, # commits ahead)

    Raises:
        ProcessError - No tracking information set for branch.
    """
    remote_name = run_cmd(['git', 'config', 'branch.%s.remote' % branch])
    merge_name = run_cmd(['git', 'config', 'branch.%s.merge' % branch])

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


def get_branch():
    """
    Determine and return the branch of the current git repository.
    If we aren't on a branch, return the prefixed hash of the current commit.
    """
    proc = Popen(['git', 'symbolic-ref', 'HEAD'], stdout=PIPE, stderr=PIPE)
    out, err = proc.communicate()
    err = err.decode('utf-8', errors='ignore').strip()
    if 'fatal: not a git repository' in err.lower():
        sys.exit(0)

    branch = out.decode('utf-8', errors='ignore').strip()[11:]
    if not branch:
        branch = SYM_PREHASH + run_cmd(['git', 'rev-parse', '--short', 'HEAD'])

    return branch


def main():
    """ Main entry point. """
    branch = get_branch()
    try:
        remote = compute_ahead_behind(branch)
    except ProcessError:
        remote = 0, 0

    values = [str(x) for x in (branch,) + remote + compute_stats()]
    sys.stdout.write(' '.join(values))
    sys.stdout.flush()


if __name__ == "__main__":
    try:
        main()
    except ProcessError:
        pass
