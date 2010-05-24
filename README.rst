Informative git prompt for zsh
==============================

About
-----

A zsh prompt that displays information about the current git repository.
In particular:

* branch name if any
* commit sha1 if not on any branch
* number of commit difference from the reference remote branch

(an original idea from this `blog entry`_).

Repository clean:

.. image:: http://files.droplr.com/files/35740123/UDMT.Picture%2014.png
	:alt: Repository clean

Branch ahead of master by one commit:

.. image:: http://files.droplr.com/files/35740123/UDQ5U.Picture15.png
	:alt: Branch ahead

Not currently on any branch:

.. image:: http://files.droplr.com/files/35740123/UDTg3.Picture16.png
	:alt: Not currently on any branch

.. _blog entry: http://sebastiancelis.com/2009/nov/16/zsh-prompt-git-users/

Symbols
-------

The symbols are as follows:

:⚡: repository clean
:↑n: ahead of remote by ``n`` commits
:↓n: behind remote by ``n`` commits
:|m↕n: branches diverged, other by ``m`` commits, yours by ``n`` commits
:♦: something is staged
:∘: something is changed
:.: some untracked files

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

#. Go in a git repository and test it!

**Enjoy!**