"""
Test module for gitstatus.

Importantly the fake repos are setup using generators.
    next(repo) - repo exists and your CWD is set to it.
    next(repo) - repo is cleaned up and you are returned to old CWD
"""
from __future__ import absolute_import, print_function
import os
import re
import shlex
import shutil
import subprocess
import tempfile  # TODO: Use tempfile instead of fixed directory name

import pytest

import gitstatus

GIT_STATUS = os.path.join(os.path.dirname(__file__), 'gitstatus.py')
# FIXME: tox -e py fails, py.test works???


def decorate_test(repo_func):
    """
    This wrapper simply provides a mechanism to setup the test repository,
    then call a func, then always cleanup.
    """
    def inner_decorator(func):
        def wrapper():
            try:
                repo = repo_func()
                next(repo)
                func()
            finally:
                try:
                    next(repo)
                except StopIteration:
                    pass

        return wrapper
    return inner_decorator


def run_gitstatus():
    """
    Helper to simply run gitstatus in the current directory.

    Returns:
        The output of gitstatus.py in the CWD.
    """
    return subprocess.check_output(['python', GIT_STATUS]).decode('utf-8', errors='ignore')


def git_repo_compute_stats():
    """
    Create a fake git repo with the following properties:
        - upstream set to another local git repo
        - 3 staged files (1 changed, 2 additions)
        - 1 changed file unstaged
        - 2 untracked files
        - 1 stashed change set
    """
    cwd = os.getcwd()
    folder = "/tmp/zsh_git"
    folder_up = folder + "_upstream"
    cmds = [
        "git init",
        "first:A single line",
        "second:A single line",
        "third:A single line",
        "touch untracked1 untracked2",
        "git add first",
        "git commit -m 'first commit'",
        "first:Changes to stash",
        "git stash",
        "first:Changes to stage",
        "git add first second third",
        "first:Changes but unstaged",
        "cp -R %s %s" % (folder, folder_up),
        "git remote add -f up %s" % folder_up,
        "git branch --set-upstream-to=up/master",
    ]
    try:
        try:
            shutil.rmtree(folder)
        except (OSError, IOError):
            pass
        try:
            shutil.rmtree(folder_up)
        except (OSError, IOError):
            pass
        os.makedirs(folder)
        os.chdir(folder)

        for cmd in cmds:
            if re.match(r'\S+:', cmd):
                assert len(cmd.split(":")) == 2
                fname, text = cmd.split(":")
                with open(os.path.join(folder, fname), 'a') as fout:
                    fout.write(text + '\n')
            else:
                with open(os.devnull, 'w') as devnull:
                    subprocess.check_call(shlex.split(cmd),
                                          stdout=devnull, stderr=subprocess.STDOUT)

        yield

    finally:
        try:
            shutil.rmtree(folder)
        except (OSError, IOError):
            pass
        try:
            shutil.rmtree(folder_up)
        except (OSError, IOError):
            pass
        os.chdir(cwd)


def git_repo_branch_on_hash():
    """
    Create a fake git repo with the following properties:
        - 3 commits made
        - yield when on checkout hash
    """
    cwd = os.getcwd()
    folder = "/tmp/zsh_git"
    cmds = [
        "git init",
        "first:A single line",
        "git add first",
        "git commit -m 'first commit'",
        "first:A second line",
        "git add first",
        "git commit -m 'second commit'",
        "git checkout HEAD~1",
    ]
    try:
        try:
            shutil.rmtree(folder)
        except (OSError, IOError):
            pass
        os.makedirs(folder)
        os.chdir(folder)

        for cmd in cmds:
            if re.match(r'\S+:', cmd):
                assert len(cmd.split(":")) == 2
                fname, text = cmd.split(":")
                with open(os.path.join(folder, fname), 'a') as fout:
                    fout.write(text + '\n')
            else:
                with open(os.devnull, 'w') as devnull:
                    subprocess.check_call(shlex.split(cmd),
                                          stdout=devnull, stderr=subprocess.STDOUT)

        yield

    finally:
        try:
            shutil.rmtree(folder)
        except (OSError, IOError):
            pass
        os.chdir(cwd)


def git_repo_branch_on_master():
    """
    Create a fake git repo with the following properties:
        - 3 commits made
        - yield when on checkout hash
    """
    cwd = os.getcwd()
    folder = "/tmp/zsh_git"
    cmds = [
        "git init",
        "first:A single line",
        "git add first",
        "git commit -m 'first commit'",
        "first:A second line",
        "git add first",
        "git commit -m 'second commit'",
    ]
    try:
        try:
            shutil.rmtree(folder)
        except (OSError, IOError):
            pass
        os.makedirs(folder)
        os.chdir(folder)

        for cmd in cmds:
            if re.match(r'\S+:', cmd):
                assert len(cmd.split(":")) == 2
                fname, text = cmd.split(":")
                with open(os.path.join(folder, fname), 'a') as fout:
                    fout.write(text + '\n')
            else:
                with open(os.devnull, 'w') as devnull:
                    subprocess.check_call(shlex.split(cmd),
                                          stdout=devnull, stderr=subprocess.STDOUT)

        yield

    finally:
        try:
            shutil.rmtree(folder)
        except (OSError, IOError):
            pass
        os.chdir(cwd)


# TODO: Use porcelain for status, saves two commands
#  conflicts = 0
#  untracked = 0
#  staged = 0
#  changed = 0
#  for line in run_cmd(['git', 'status', '--porcelain']).splitlines():
    #  if line[0] in ['A', 'M']:
        #  changed += 1
    #  if line[0] in ['A', 'M']:
        #  staged += 1
    #  if line[0:2] == '??':
        #  untracked += 1


def test_run_cmd():
    out = gitstatus.run_cmd(["echo", "It works"])
    assert out == "It works"


def test_run_cmd_fail():
    with pytest.raises(gitstatus.ProcessError):
        gitstatus.run_cmd(["false"])


def test_branch_fatal():
    cwd = os.getcwd()
    try:
        folder = tempfile.mkdtemp()
        os.chdir(folder)
        try:
            gitstatus.get_branch()
            assert False
        except SystemExit:
            pass
    finally:
        os.chdir(cwd)
        try:
            shutil.rmtree(folder)
        except (OSError, IOError):
            pass


@decorate_test(git_repo_branch_on_master)
def test_branch_master():
    assert gitstatus.get_branch() == 'master'


@decorate_test(git_repo_branch_on_hash)
def test_branch_hash():
    actual_hash = gitstatus.run_cmd(shlex.split('git rev-parse --short HEAD'))
    assert gitstatus.get_branch() == gitstatus.SYM_PREHASH + actual_hash


@decorate_test(git_repo_compute_stats)
def test_compute_stats_no_conflicts():
    assert run_gitstatus() == 'master 0 0 3 0 1 2 1'
