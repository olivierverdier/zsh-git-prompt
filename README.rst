Informative git prompt for zsh
==============================

A ``zsh`` prompt that displays information about the current git repository.
In particular the branch name, difference with remote branch, number of files staged, changed, etc.

(an original idea from this `blog entry`_).

Examples
--------

The prompt may look like the following: 

* ``(master↑3‣1)``: on branch ``master``, ahead of remote by 3 commits, 1 file changed but not staged
* ``(status♦2)``: on branch ``status``, 2 files staged
* ``(master‣7…)``: on branch ``master``, 7 files changed, some files untracked
* ``(experimental↓2↑3)``: on branch ``experimental``; your branch has diverged by 3 commits, remote by 2 commits
* ``(:70c2952)``: not on any branch; parent commit has sha1 ``70c2952``

Here is how it could look like when you are ahead by 2 commits, and have 3 staged files, 1 changed but unstaged file, and some untracked files, on branch ``dev``:

.. image:: http://files.droplr.com.s3.amazonaws.com/files/35740123/14GMbk.Picture%2023.png
	:alt: Example


.. _blog entry: http://sebastiancelis.com/2009/nov/16/zsh-prompt-git-users/

Symbols
-------

The symbols are as follows:

* Status Symbols
	:⚡: repository clean
	:♦n: there are ``n`` staged files
	:♠n: there are ``n`` unmerged files
	:‣n: there are ``n`` changed but *unstaged* files
	:…: there are some untracked files

* Branch Symbols
	:↑n: ahead of remote by ``n`` commits
	:↓n: behind remote by ``n`` commits
	:↓m↑n: branches diverged, other by ``m`` commits, yours by ``n`` commits
	:\:: when the branch name starts with a colon ``:``, it means it's actually a sha1, not a branch (although it should be pretty clear, unless you name your branches like sha1 :-)

Install
-------

#. Copy the contents of ``zshrc.sh`` into your ``~/.zshrc`` config file (and adapt the ``PROMPT`` variable to your needs)
#. Create the directories ``~/.zsh`` and ``~/.zsh/functions`` if they do not exist
#. Move the files so as to get the following structure::

	.zsh/
		gitstatus.py
		functions/
			chpwd_update_git_vars.sh
			precmd_update_git_vars.sh
			preexec_update_git_vars.sh
			prompt_git_info
			update_current_git_vars.sh

#. Adapt the format of the git information inside the ``prompt_git_info`` file to your needs.
#. Go in a git repository and test it!

**Enjoy!**