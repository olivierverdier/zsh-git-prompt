#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
Retrieve information about the git repository in CWD.

Invoked by ./zshrc.sh automatically.
"""
import os
import subprocess as sub
import sys

# Denotes no upstream set, impossible branch name per git ref spec
SYM_NOUPSTREAM = '..'
# This symbol appears before hashes when detached
SYM_PREHASH = os.environ.get('ZSH_THEME_GIT_PROMPT_HASH_PREFIX', ':')


# TODO: Naiev fix, consider work trees too.
def find_git_root():
    """
    Find the nearest enclosing git root.
    Condition: Will be called from within a git respository.

    Returns: The git project root.

    Raises: IOError - Could not find the directory.
    """
    working_d = os.getcwd()
    while working_d != '/':
        git_d = os.path.join(working_d, '.git')
        if os.path.exists(git_d):
            return git_d
        working_d = os.path.dirname(working_d)

    raise IOError("No git dir in folder hierarchy.")


def parse_stats(lines):
    """
    Computes and returns the following _numbers_ describing the current state.
        number of staged files
        number of conflicts
        number of changed files
        number of untracked files

    Returns:
        (# staged, # conflicts, # changed, # untracked)
    """
    staged, conflicts, changed, untracked = 0, 0, 0, 0

    # See status format in docs: https://git-scm.com/docs/git-status
    for line in lines:
        if line[0:2] == '??':
            untracked += 1
            continue
        if line[0:2] in ['AA', 'AU', 'DD', 'DU', 'UA', 'UD', 'UU']:
            conflicts += 1
            continue
        if line[0] in ['A', 'C', 'D', 'M', 'R']:
            staged += 1
        if line[1] in ['C', 'D', 'M', 'R']:
            changed += 1

    return staged, conflicts, changed, untracked


def parse_ahead_behind(branch):
    """
    Parse how far ahead and/or behind the branch and remote are.

    Args:
        branch: The branch line of porcelain

    Returns:
        (# commits behind, # commits ahead)
    """
    ahead, behind = 0, 0
    if branch[-1] == ']':
        for part in branch[branch.rindex('[') + 1:-1].split(','):
            if 'ahead' in part:
                ahead = int(part.replace('ahead ', ''))
            else:
                behind = int(part.replace('behind ', ''))

    return behind, ahead


def parse_branch(branch):
    """
    Determine the current state of HEAD (on a branch or checked out on hash).
    Determine if the branch has an upstream set.
    Determine if the branch is local only.
    Capture status for later processing.

    Args:
        branch: The branch line of porcelain

    Returns: A tuple of following ...
        branch: Set to the actual branch name or the hash we are on.
        upstream: Set to the upstream branch if tracked else SYM_NOUPSTREAM.
        local: 1 IFF the branch has no upstream and is not checked out hash.
    """
    branch = branch[3:]
    if ' [' in branch:
        branch = branch[:branch.rindex(' [')]

    upstream = SYM_NOUPSTREAM
    local = 1
    if 'no branch' in branch:
        with open(os.path.join(GIT_D, 'HEAD')) as fin:
            branch = SYM_PREHASH + fin.read().strip()[:7]
        local = 0
    elif '...' in branch:
        branch, upstream = branch.split('...')
        local = 0

    return branch, upstream, local


def current_git_status(lines):
    """
    Parse git status procelain output and return the formatted text that
    represents the current status of the respoistory.

    Returns: The formatted message representing the git repository.
    """
    # TODO: Use upstream and update tests
    branch, _, local = parse_branch(lines[0])
    remote = parse_ahead_behind(lines[0])
    stats = parse_stats(lines[1:])

    try:
        with open(os.path.join(GIT_D, 'logs', 'refs', 'stash')) as fin:
            stashed = len(fin.readlines())
    except IOError:
        stashed = 0

    values = [str(x) for x in (branch,) + remote + stats + (stashed, local)]

    return ' '.join(values)


def main():
    """
    This program can be run two ways:
        1) `./gitstatus.py`
            Will wait on subprocess to execute below git status command.

        2) `git status --branch --porcelain | ./gitstatus.py`
            Will read stdin and parse it.
    """
    if not sys.stdin.isatty():
        lines = [line.rstrip() for line in sys.stdin.readlines()]
        err = u'\n'.join(lines)
    else:
        proc = sub.Popen(['git', 'status', '--branch', '--porcelain'],
                         stdout=sub.PIPE, stderr=sub.PIPE)
        out, err = proc.communicate()
        err = err.decode('utf-8', errors='ignore').strip()
        lines = out.decode('utf-8', errors='ignore').splitlines()

    if 'fatal: not a git repository' in err.lower():
        sys.exit(0)

    sys.stdout.write(current_git_status(lines))
    sys.stdout.flush()


try:
    GIT_D = find_git_root()
except IOError:
    pass


if __name__ == "__main__":
    main()
