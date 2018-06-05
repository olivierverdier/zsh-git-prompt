"""
Test module for gitstatus

Fixtures used to to setup git repo scenarios on the fly.
Tests are short and at the end of this file.
"""
from __future__ import absolute_import, print_function
import os
import re
import shlex
import shutil
import subprocess as sub
import tempfile

import pytest

import gitstatus

GIT_STATUS = os.path.join(os.path.dirname(__file__), 'gitstatus.py')


def run_gitstatus():
    """
    Helper to simply run gitstatus in the current directory.

    Returns:
        The output of gitstatus.py in the CWD.
    """
    return sub.check_output(['python', GIT_STATUS]).decode('utf-8', errors='ignore')


@pytest.yield_fixture(scope="function")
def empty_working_directory():
    """
    Run a test inside an empty temporary directory.
    """
    cwd = os.getcwd()
    try:
        folder = tempfile.mkdtemp()
        os.chdir(folder)

        yield
    finally:
        os.chdir(cwd)
        try:
            shutil.rmtree(folder)
        except (OSError, IOError):
            pass


@pytest.yield_fixture(scope="function")
def git_repo_initial_commit():
    """
    Create a fake git repo with the following properties:
        - No commits beyond initialization.
    """
    cwd = os.getcwd()
    folder = tempfile.mkdtemp()
    cmds = [
        "git init",
        "git config user.email 'you@example.com'",
        "git config user.name 'Your Name'",
    ]
    try:
        os.chdir(folder)

        for cmd in cmds:
            if re.match(r'\S+:', cmd):
                assert len(cmd.split(":")) == 2
                fname, text = cmd.split(":")
                with open(os.path.join(folder, fname), 'a') as fout:
                    fout.write(text + '\n')
            else:
                with open(os.devnull, 'w') as devnull:
                    sub.check_call(shlex.split(cmd),
                                   stdout=devnull, stderr=sub.STDOUT)

        yield

    finally:
        try:
            shutil.rmtree(folder)
        except (OSError, IOError):
            pass
        os.chdir(cwd)


@pytest.yield_fixture(scope="function")
def git_repo_find_git_root():
    """
    Create a fake git repo with the following properties:
        - 1 commit
        - nested folders called, first/second/third
    """
    cwd = os.getcwd()
    folder = tempfile.mkdtemp()
    cmds = [
        "git init",
        "git config user.email 'you@example.com'",
        "git config user.name 'Your Name'",
        "first:A single line",
        "git add first",
        "git commit -m 'first commit'",
    ]
    try:
        subs = os.path.join(folder, 'd_one', 'd_two', 'd_three')
        os.makedirs(subs)
        os.chdir(folder)

        for cmd in cmds:
            if re.match(r'\S+:', cmd):
                assert len(cmd.split(":")) == 2
                fname, text = cmd.split(":")
                with open(os.path.join(folder, fname), 'a') as fout:
                    fout.write(text + '\n')
            else:
                with open(os.devnull, 'w') as devnull:
                    sub.check_call(shlex.split(cmd),
                                   stdout=devnull, stderr=sub.STDOUT)

        yield

    finally:
        try:
            shutil.rmtree(folder)
        except (OSError, IOError):
            pass
        os.chdir(cwd)


@pytest.yield_fixture(scope="function")
def git_repo_with_worktree():
    """
    Create a fake git repo with the following properties:
        - main repo has 3 commits
        - upstream repo has 3 commits
        - main repo has upstream set and is has diverged by 1 commit each way
    """
    cwd = os.getcwd()
    folder = tempfile.mkdtemp()
    folder_tree = folder + "_worktree"
    cmds = [
        "git init",
        "git config user.email 'you@example.com'",
        "git config user.name 'Your Name'",
        "first:A single line",
        "git add first",
        "git commit -m 'first commit'",
        "first:Second line",
        "git add first",
        "git commit -m 'second commit'",
        "git branch tree",
        "git checkout tree",
        "first:third line",
        "git add first",
        "git commit -m 'third commit'",
        "git checkout master",
        "git worktree add --detach %s tree" % (folder_tree),
    ]
    try:
        os.chdir(folder)

        for cmd in cmds:
            if re.match(r'\S+:', cmd):
                assert len(cmd.split(":")) == 2
                fname, text = cmd.split(":")
                with open(os.path.join(folder, fname), 'a') as fout:
                    fout.write(text + '\n')
            else:
                with open(os.devnull, 'w') as devnull:
                    sub.check_call(shlex.split(cmd),
                                   stdout=devnull, stderr=sub.STDOUT)

        os.chdir(folder_tree)
        yield

    finally:
        try:
            shutil.rmtree(folder)
        except (OSError, IOError):
            pass
        try:
            shutil.rmtree(folder_tree)
        except (OSError, IOError):
            pass
        os.chdir(cwd)


@pytest.yield_fixture(scope="function")
def git_repo_parse_stats():
    """
    Create a fake git repo with the following properties:
        - upstream set to another local git repo
        - 3 staged files (1 changed, 2 additions)
        - 1 changed file unstaged
        - 2 untracked files
        - 1 stashed change set
    """
    cwd = os.getcwd()
    folder = tempfile.mkdtemp()
    folder_up = folder + "_upstream"
    cmds = [
        "git init",
        "git config user.email 'you@example.com'",
        "git config user.name 'Your Name'",
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
        os.chdir(folder)

        for cmd in cmds:
            if re.match(r'\S+:', cmd):
                assert len(cmd.split(":")) == 2
                fname, text = cmd.split(":")
                with open(os.path.join(folder, fname), 'a') as fout:
                    fout.write(text + '\n')
            else:
                with open(os.devnull, 'w') as devnull:
                    sub.check_call(shlex.split(cmd),
                                   stdout=devnull, stderr=sub.STDOUT)

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


@pytest.yield_fixture(scope="function")
def git_repo_parse_stats_only_conflicts():
    """
    Create a fake git repo with the following properties:
        - upstream set to another local git repo
        - edit the same file and create a merge conflict
    """
    cwd = os.getcwd()
    folder = tempfile.mkdtemp()
    folder_up = folder + "_upstream"
    cmds = [
        "git init",
        "git config user.email 'you@example.com'",
        "git config user.name 'Your Name'",
        "first:A single line\nsecond line\third line",
        "git add first",
        "git commit -m 'first commit'",
        "first:fourth line\nfifth line\n",
        "git add first",
        "git commit -m 'second commit'",
        "cp -R %s %s" % (folder, folder_up),
        "git reset --hard HEAD~1",
        "first:ninth line\ntenth line\n",
        "git add first",
        "git commit -m 'new second commit'",
        "git remote add -f up %s" % folder_up,
        "git branch --set-upstream-to=up/master",
        "git fetch up",
        "git merge up/master",
    ]
    try:
        os.chdir(folder)

        for cmd in cmds:
            if re.match(r'\S+:', cmd):
                assert len(cmd.split(":")) == 2
                fname, text = cmd.split(":")
                with open(os.path.join(folder, fname), 'a') as fout:
                    fout.write(text + '\n')
            else:
                try:
                    with open(os.devnull, 'w') as devnull:
                        sub.check_call(shlex.split(cmd),
                                       stdout=devnull, stderr=sub.STDOUT)
                except sub.CalledProcessError:
                    pass

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


@pytest.yield_fixture(scope="function")
def git_repo_branch_on_hash():
    """
    Create a fake git repo with the following properties:
        - 3 commits made
        - yield when on checkout hash
    """
    cwd = os.getcwd()
    folder = tempfile.mkdtemp()
    cmds = [
        "git init",
        "git config user.email 'you@example.com'",
        "git config user.name 'Your Name'",
        "first:A single line",
        "git add first",
        "git commit -m 'first commit'",
        "first:A second line",
        "git add first",
        "git commit -m 'second commit'",
        "git checkout HEAD~1",
    ]
    try:
        os.chdir(folder)

        for cmd in cmds:
            if re.match(r'\S+:', cmd):
                assert len(cmd.split(":")) == 2
                fname, text = cmd.split(":")
                with open(os.path.join(folder, fname), 'a') as fout:
                    fout.write(text + '\n')
            else:
                with open(os.devnull, 'w') as devnull:
                    sub.check_call(shlex.split(cmd),
                                   stdout=devnull, stderr=sub.STDOUT)

        yield

    finally:
        try:
            shutil.rmtree(folder)
        except (OSError, IOError):
            pass
        os.chdir(cwd)


@pytest.yield_fixture(scope="function")
def git_repo_branch_on_master():
    """
    Create a fake git repo with the following properties:
        - 3 commits made
        - yield when on checkout hash
    """
    cwd = os.getcwd()
    folder = tempfile.mkdtemp()
    cmds = [
        "git init",
        "git config user.email 'you@example.com'",
        "git config user.name 'Your Name'",
        "first:A single line",
        "git add first",
        "git commit -m 'first commit'",
        "first:A second line",
        "git add first",
        "git commit -m 'second commit'",
    ]
    try:
        os.chdir(folder)

        for cmd in cmds:
            if re.match(r'\S+:', cmd):
                assert len(cmd.split(":")) == 2
                fname, text = cmd.split(":")
                with open(os.path.join(folder, fname), 'a') as fout:
                    fout.write(text + '\n')
            else:
                with open(os.devnull, 'w') as devnull:
                    sub.check_call(shlex.split(cmd),
                                   stdout=devnull, stderr=sub.STDOUT)

        yield

    finally:
        try:
            shutil.rmtree(folder)
        except (OSError, IOError):
            pass
        os.chdir(cwd)


@pytest.yield_fixture(scope="function")
def git_repo_branch_local_only():
    """
    Create a fake git repo with the following properties:
        - 1 commit
        - no upstream copy or set value
    """
    cwd = os.getcwd()
    folder = tempfile.mkdtemp()
    cmds = [
        "git init",
        "git config user.email 'you@example.com'",
        "git config user.name 'Your Name'",
        "first:A single line",
        "git add first",
        "git commit -m 'first commit'",
    ]
    try:
        os.chdir(folder)

        for cmd in cmds:
            if re.match(r'\S+:', cmd):
                assert len(cmd.split(":")) == 2
                fname, text = cmd.split(":")
                with open(os.path.join(folder, fname), 'a') as fout:
                    fout.write(text + '\n')
            else:
                with open(os.devnull, 'w') as devnull:
                    sub.check_call(shlex.split(cmd),
                                   stdout=devnull, stderr=sub.STDOUT)

        yield

    finally:
        try:
            shutil.rmtree(folder)
        except (OSError, IOError):
            pass
        os.chdir(cwd)


@pytest.yield_fixture(scope="function")
def git_repo_remote_ahead():
    """
    Create a fake git repo with the following properties:
        - main repo has 3 commits
        - upstream repo has 2 commits
        - main repo has upstream set and is AHEAD by 1 commit
    """
    cwd = os.getcwd()
    folder = tempfile.mkdtemp()
    folder_up = folder + "_upstream"
    cmds = [
        "git init",
        "git config user.email 'you@example.com'",
        "git config user.name 'Your Name'",
        "first:A single line",
        "git add first",
        "git commit -m 'first commit'",
        "first:Second line",
        "git add first",
        "git commit -m 'second commit'",
        "cp -R %s %s" % (folder, folder_up),
        "first:third line",
        "git add first",
        "git commit -m 'third commit'",
        "git remote add -f up %s" % folder_up,
        "git branch --set-upstream-to=up/master",
    ]
    try:
        os.chdir(folder)

        for cmd in cmds:
            if re.match(r'\S+:', cmd):
                assert len(cmd.split(":")) == 2
                fname, text = cmd.split(":")
                with open(os.path.join(folder, fname), 'a') as fout:
                    fout.write(text + '\n')
            else:
                with open(os.devnull, 'w') as devnull:
                    sub.check_call(shlex.split(cmd),
                                   stdout=devnull, stderr=sub.STDOUT)

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


@pytest.yield_fixture(scope="function")
def git_repo_remote_behind():
    """
    Create a fake git repo with the following properties:
        - main repo has 2 commits
        - upstream repo has 3 commits
        - main repo has upstream set and is BEHIND by 1 commit
    """
    cwd = os.getcwd()
    folder = tempfile.mkdtemp()
    folder_up = folder + "_upstream"
    cmds = [
        "git init",
        "git config user.email 'you@example.com'",
        "git config user.name 'Your Name'",
        "first:A single line",
        "git add first",
        "git commit -m 'first commit'",
        "first:Second line",
        "git add first",
        "git commit -m 'second commit'",
        "first:third line",
        "git add first",
        "git commit -m 'third commit'",
        "cp -R %s %s" % (folder, folder_up),
        "git remote add -f up %s" % folder_up,
        "git branch --set-upstream-to=up/master",
        "git reset --hard HEAD~1",
    ]
    try:
        os.chdir(folder)

        for cmd in cmds:
            if re.match(r'\S+:', cmd):
                assert len(cmd.split(":")) == 2
                fname, text = cmd.split(":")
                with open(os.path.join(folder, fname), 'a') as fout:
                    fout.write(text + '\n')
            else:
                with open(os.devnull, 'w') as devnull:
                    sub.check_call(shlex.split(cmd),
                                   stdout=devnull, stderr=sub.STDOUT)

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


@pytest.yield_fixture(scope="function")
def git_repo_remote_diverged():
    """
    Create a fake git repo with the following properties:
        - main repo has 3 commits
        - upstream repo has 3 commits
        - main repo has upstream set and is has diverged by 1 commit each way
    """
    cwd = os.getcwd()
    folder = tempfile.mkdtemp()
    folder_up = folder + "_upstream"
    cmds = [
        "git init",
        "git config user.email 'you@example.com'",
        "git config user.name 'Your Name'",
        "first:A single line",
        "git add first",
        "git commit -m 'first commit'",
        "first:Second line",
        "git add first",
        "git commit -m 'second commit'",
        "first:third line",
        "git add first",
        "git commit -m 'third commit'",
        "cp -R %s %s" % (folder, folder_up),
        "git remote add -f up %s" % folder_up,
        "git branch --set-upstream-to=up/master",
        "git reset --hard HEAD~1",
        "first:different third line",
        "git add first",
        "git commit -m 'different third commit'",
    ]
    try:
        os.chdir(folder)

        for cmd in cmds:
            if re.match(r'\S+:', cmd):
                assert len(cmd.split(":")) == 2
                fname, text = cmd.split(":")
                with open(os.path.join(folder, fname), 'a') as fout:
                    fout.write(text + '\n')
            else:
                with open(os.devnull, 'w') as devnull:
                    sub.check_call(shlex.split(cmd),
                                   stdout=devnull, stderr=sub.STDOUT)

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


@pytest.yield_fixture(scope="function")
def git_repo_in_merge():
    """
    Create a fake git repo with the following properties:
        - master branch with 2 commits
        - dev branch that has 2 commits, last one differs from master
        - dev branch is merging master into it
    """
    cwd = os.getcwd()
    folder = tempfile.mkdtemp()
    cmds = [
        "git init",
        "git config user.email 'you@example.com'",
        "git config user.name 'Your Name'",
        "first:A single line",
        "git add first",
        "git commit -m 'first commit'",
        "git branch dev",
        "first:the second master line here",
        "git add first",
        "git commit -m 'second master commit'",
        "git checkout dev",
        "first:Second line for dev",
        "git add first",
        "git commit -m 'second dev commit'",
        "git merge master",
    ]
    try:
        os.chdir(folder)

        for cmd in cmds:
            if re.match(r'\S+:', cmd):
                assert len(cmd.split(":")) == 2
                fname, text = cmd.split(":")
                with open(os.path.join(folder, fname), 'a') as fout:
                    fout.write(text + '\n')
            else:
                with open(os.devnull, 'w') as devnull:
                    proc = sub.Popen(shlex.split(cmd),
                                     stdout=devnull, stderr=sub.STDOUT)
                    proc.wait()

        yield

    finally:
        try:
            shutil.rmtree(folder)
        except (OSError, IOError):
            pass
        os.chdir(cwd)


@pytest.yield_fixture(scope="function")
def git_repo_in_rebase():
    """
    Create a fake git repo with the following properties:
        - master branch with 3 commits
        - dev branch that has 3 commits, last two differ from master
        - dev is rebasing master, 2 commits need resolving
    """
    cwd = os.getcwd()
    folder = tempfile.mkdtemp()
    cmds = [
        "git init",
        "git config user.email 'you@example.com'",
        "git config user.name 'Your Name'",
        "first:A single line",
        "git add first",
        "git commit -m 'first commit'",
        "git branch dev",
        "first:the second master line here",
        "git add first",
        "git commit -m 'second master commit'",
        "first:there is also a third master",
        "git add first",
        "git commit -m 'third master commit'",
        "git checkout dev",
        "first:Second line",
        "git add first",
        "git commit -m 'second dev commit'",
        "first:Third line\nForuth line",
        "git add first",
        "git commit -m 'third dev commit'",
        "git rebase master",
    ]
    try:
        os.chdir(folder)

        for cmd in cmds:
            if re.match(r'\S+:', cmd):
                assert len(cmd.split(":")) == 2
                fname, text = cmd.split(":")
                with open(os.path.join(folder, fname), 'a') as fout:
                    fout.write(text + '\n')
            else:
                with open(os.devnull, 'w') as devnull:
                    proc = sub.Popen(shlex.split(cmd),
                                     stdout=devnull, stderr=sub.STDOUT)
                    proc.wait()

        yield

    finally:
        try:
            shutil.rmtree(folder)
        except (OSError, IOError):
            pass
        os.chdir(cwd)


# ----------------
# Functional Tests
# ----------------
def test_find_git_root(git_repo_find_git_root):
    """ A unit test for gitstatus. """
    expect = os.path.join(os.getcwd(), '.git')
    sub_d = os.path.join(os.getcwd(), 'd_one', 'd_two', 'd_three')
    assert os.path.isdir(sub_d)
    os.chdir(sub_d)
    assert gitstatus.find_git_root() == expect


def test_find_git_root_fail(empty_working_directory):
    """ A unit test for gitstatus. """
    with pytest.raises(IOError):
        gitstatus.find_git_root()


def test_git_paths_in_normal_repo(git_repo_initial_commit):
    """ A unit test for gitstatus. """
    head_file, stash_file, merge_file, rebase_dir = gitstatus.git_paths(gitstatus.find_git_root())
    assert head_file == os.path.join(os.getcwd(), '.git', 'HEAD')
    assert stash_file == os.path.join(os.getcwd(), '.git', 'logs', 'refs', 'stash')
    assert merge_file == os.path.join(os.getcwd(), '.git', 'MERGE_HEAD')
    assert rebase_dir == os.path.join(os.getcwd(), '.git', 'rebase-apply')


def test_git_paths_in_working_tree(git_repo_with_worktree):
    """ A unit test for gitstatus. """
    repo_root = os.getcwd().replace('_worktree', '')
    tree_root = os.path.join(repo_root, '.git', 'worktrees',
                             os.path.basename(repo_root) + '_worktree')
    head_file, stash_file, merge_file, rebase_dir = gitstatus.git_paths(gitstatus.find_git_root())
    assert head_file == os.path.join(tree_root, 'HEAD')
    assert stash_file == os.path.join(repo_root, '.git', 'logs', 'refs', 'stash')
    assert merge_file == os.path.join(tree_root, 'MERGE_HEAD')
    assert rebase_dir == os.path.join(tree_root, 'rebase-apply')


def test_parse_stats():
    """ A unit test for gitstatus. """
    status_input = """?? untracked1
?? untracked2
?? untracked3
AA conflicts1
AU conflicts2
DD conflicts3
DU conflicts4
UA conflicts5
UD conflicts6
UD conflicts7
A_ staged1
C_ staged2
D_ staged3
M_ staged4
R_ staged5
_C changed1
_D changed2
_M changed3
_R changed4"""
    assert gitstatus.parse_stats(status_input.splitlines()) == (5, 7, 4, 3)


def test_parse_ahead_behind_only_ahead():
    """ A unit test for gitstatus. """
    assert gitstatus.parse_ahead_behind("## master...up/master [ahead 2]") == (0, 2)


def test_parse_ahead_behind_only_behind():
    """ A unit test for gitstatus. """
    assert gitstatus.parse_ahead_behind("## master...up/master [behind 1]") == (1, 0)


def test_parse_ahead_behind_both():
    """ A unit test for gitstatus. """
    assert gitstatus.parse_ahead_behind("## master...up/master [ahead 2, behind 1]") == (1, 2)


def test_parse_branch_on_local_branch():
    """ A unit test for gitstatus. """
    branch_line = "## master"
    assert gitstatus.parse_branch(branch_line, None) == ('master', '..', 1)


def test_parse_branch_has_upstream():
    """ A unit test for gitstatus. """
    branch_line = "## master...up/master [ahead 2, behind 1]"
    assert gitstatus.parse_branch(branch_line, None) == ('master', 'up/master', 0)


def test_parse_branch_out_on_hash(git_repo_branch_on_hash):
    """ A unit test for gitstatus. """
    actual_hash = sub.check_output(shlex.split('git rev-parse --short HEAD'))
    actual_hash = actual_hash.decode('utf-8', errors='ignore').strip()
    head_file = os.path.join(os.getcwd(), '.git', 'HEAD')
    branch_line = "## HEAD (no branch)"
    assert gitstatus.parse_branch(branch_line, head_file) == (':' + actual_hash, '..', 0)


def test_stash_count_one_stash(git_repo_parse_stats):
    """ A unit test for gitstatus. """
    stash_file = os.path.join(os.getcwd(), '.git', 'logs', 'refs', 'stash')
    assert gitstatus.stash_count(stash_file) == 1


def test_stash_count_no_stash(git_repo_initial_commit):
    """ A unit test for gitstatus. """
    stash_file = os.path.join(os.getcwd(), 'logs', 'refs', 'stash')
    assert gitstatus.stash_count(stash_file) == 0


def test_rebase_progress_active_rebase(git_repo_in_rebase):
    rebase_dir = os.path.join(os.getcwd(), '.git', 'rebase-apply')
    assert gitstatus.rebase_progress(rebase_dir) == '1/2'


def test_rebase_progress_no_rebase(git_repo_initial_commit):
    rebase_dir = os.path.join(os.getcwd(), '.git', 'rebase-apply')
    assert gitstatus.rebase_progress(rebase_dir) == '0'


# -----------------
# Integration Tests
# -----------------
def test_gitstatus_no_repo(empty_working_directory):
    """ A unit test for gitstatus. """
    assert run_gitstatus() == ''


def test_gitstatus_initial_commit(git_repo_initial_commit):
    """ A unit test for gitstatus. """
    assert run_gitstatus() == 'master 0 0 0 0 0 0 0 1 {} 0 0'.format(gitstatus.SYM_NOUPSTREAM)


def test_gitstatus_local_branch(git_repo_branch_on_master):
    """ A unit test for gitstatus. """
    assert run_gitstatus() == 'master 0 0 0 0 0 0 0 1 {} 0 0'.format(gitstatus.SYM_NOUPSTREAM)


def test_gitstatus_on_hash(git_repo_branch_on_hash):
    """ A unit test for gitstatus. """
    actual_hash = sub.check_output(shlex.split('git rev-parse --short HEAD'))
    actual_hash = actual_hash.decode('utf-8', errors='ignore').strip()
    assert run_gitstatus() == ':{} 0 0 0 0 0 0 0 0 {} 0 0'.format(actual_hash,
                                                                  gitstatus.SYM_NOUPSTREAM)


def test_gitstatus_parse_stats_no_conflicts(git_repo_parse_stats):
    """ A unit test for gitstatus. """
    assert run_gitstatus() == 'master 0 0 3 0 1 2 1 0 up/master 0 0'


def test_gitstatus_parse_stats_only_conflicts(git_repo_parse_stats_only_conflicts):
    """ A unit test for gitstatus. """
    assert run_gitstatus() == 'master 1 1 0 1 0 0 0 0 up/master 1 0'


def test_gitstatus_remote_ahead(git_repo_remote_ahead):
    """ A unit test for gitstatus. """
    assert run_gitstatus() == 'master 0 1 0 0 0 0 0 0 up/master 0 0'


def test_gitstatus_remote_behind(git_repo_remote_behind):
    """ A unit test for gitstatus. """
    assert run_gitstatus() == 'master 1 0 0 0 0 0 0 0 up/master 0 0'


def test_gitstatus_remote_diverged(git_repo_remote_diverged):
    """ A unit test for gitstatus. """
    assert run_gitstatus() == 'master 1 1 0 0 0 0 0 0 up/master 0 0'


def test_gitstatus_stdin(git_repo_parse_stats):
    """ A unit test for gitstatus. """
    std_input = sub.check_output(['git', 'status', '--branch', '--porcelain'])
    with tempfile.TemporaryFile() as finput:
        finput.write(std_input)
        finput.seek(0)
        out = sub.check_output(['python', GIT_STATUS], stdin=finput).decode('utf-8')
    assert out == 'master 0 0 3 0 1 2 1 0 up/master 0 0'


def test_gitstatus_merging(git_repo_in_merge):
    """ A unit test for gitstatus. """
    assert run_gitstatus() == 'dev 0 0 0 1 0 0 0 1 .. 1 0'


def test_gitstatus_rebasing(git_repo_in_rebase):
    """ A unit test for gitstatus. """
    actual_hash = sub.check_output(shlex.split('git rev-parse --short HEAD'))
    actual_hash = actual_hash.decode('utf-8', errors='ignore').strip()
    assert run_gitstatus() == ':{} 0 0 0 1 0 0 0 0 .. 0 1/2'.format(actual_hash)
