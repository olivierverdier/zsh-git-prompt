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


def find_git_root():
    """
    Find the nearest enclosing git root.

    Returns: The git project root.

    Raises:
        IOError: There is no `.git` folder in the current folder hierarchy.
    """
    working_d = os.getcwd()
    while working_d != '/':
        git_d = os.path.join(working_d, '.git')
        if os.path.exists(git_d):
            return git_d
        working_d = os.path.dirname(working_d)

    raise IOError("No git dir in folder hierarchy.")


# Example contents of worktree `.git` file, worktree is w1
#       gitdir: /tmp/zsh-git-prompt/.git/worktrees/w1
def git_paths(git_root):
    """
    Determine the location of the head file and stash file for the current repository.
    This function takes into account if we are currently in a worktree.
    Paths will be absolute to location in the ORIGINAL repository.

    Returns: (head_file, stash_file)
    """
    if os.path.isdir(git_root):
        head_file = os.path.join(git_root, 'HEAD')
    else:  # worktree
        with open(git_root) as fin:
            tree_d = fin.read().split(": ")[1].strip()

        git_root = tree_d
        while os.path.basename(git_root) != '.git':
            git_root = os.path.dirname(git_root)

        head_file = os.path.join(tree_d, 'HEAD')

    stash_file = os.path.join(git_root, 'logs', 'refs', 'stash')

    return head_file, stash_file


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


def parse_branch(branch, head_file):
    """
    Determine the current state of HEAD (on a branch or checked out on hash).
    Determine if the branch has an upstream set.
    Determine if the branch is local only.

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
    if '...' in branch:
        branch, upstream = branch.split('...')
        local = 0
    elif 'no branch' in branch:
        with open(head_file) as fin:
            branch = SYM_PREHASH + fin.read().strip()[:7]
        local = 0
    elif branch.startswith('Initial commit') or branch.startswith('No commits yet'):
        branch = branch.split(' ')[-1]

    return branch, upstream, local


def current_git_status(lines):
    """
    Parse git status procelain output and return the formatted text that
    represents the current status of the respoistory.

    Returns: The formatted message representing the git repository.

    Raises:
        IOError: There is no `.git` folder in the current folder hierarchy.
    """
    head_file, stash_file = git_paths(find_git_root())
    branch, upstream, local = parse_branch(lines[0], head_file)
    remote = parse_ahead_behind(lines[0])
    stats = parse_stats(lines[1:])

    try:
        with open(stash_file) as fin:
            stashed = len(fin.readlines())
    except IOError:
        stashed = 0

    values = [str(x) for x in (branch,) + remote + stats + (stashed, local, upstream)]

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

    if err.lower().startswith('fatal: not a git repository'):
        return

    try:
        sys.stdout.write(current_git_status(lines))
        sys.stdout.flush()
    except IOError:  # pragma: no cover
        pass


if __name__ == "__main__":
    main()
