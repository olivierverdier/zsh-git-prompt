# Informative git prompt for zsh

[![Build Status](https://travis-ci.org/starcraftman/zsh-git-prompt.svg?branch=master)](https://travis-ci.org/starcraftman/zsh-git-prompt)

A `zsh` prompt that displays information about the current git repository. In particular the branch name, difference with remote branch, number of files staged, changed, etc.

(an original idea from this [blog post][]).

## Active Fork

This is an active fork of olivierverdier/zsh-git-prompt

The original maintainer is inactive, I aim to maintain and extend the original with new features.

I do not write Haskell, so I can only ensure the `zshrc.sh` and python version are working. Please contribute PRs for Haskell parity. See issue #5 .

A summary of all changes can be found at [Fork Status](https://github.com/starcraftman/zsh-git-prompt/wiki)

## Examples

The prompt may look like the following:

-   `(master↑3|✚1)`: on branch `master`, ahead of remote by 3 commits, 1 file changed but not staged
-   `(status|●2)`: on branch `status`, 2 files staged
-   `(master|✚7…)`: on branch `master`, 7 files changed, some files untracked
-   `(master|✖2✚3)`: on branch `master`, 2 conflicts, 3 files changed
-   `(experimental↓2↑3|✔)`: on branch `experimental`; your branch has diverged by 3 commits, remote by 2 commits; the repository is otherwise clean
-   `(:70c2952|✔)`: not on any branch; parent commit has hash `70c2952`;
    the repository is otherwise clean

Here is how it could look like when you are ahead by 4 commits, behind by 5 commits, and have 1 staged files, 1 changed but unstaged file, and some untracked files, on branch `dev`:


<img src="https://github.com/olivierverdier/zsh-git-prompt/raw/master/screenshot.png" width=300/>


## Prompt Structure

By default, the general appearance of the prompt is:

```
(<branch><branch tracking>|<local status>)
```

The symbols are as follows:

### Local Status Symbols

|Symbol |Meaning
|------ |-------
|✔      |repository clean
|●n     |there are `n` staged files
|✖n     |there are `n` unmerged files
|✚n     |there are `n` changed but *unstaged* files
|…n     |there are `n` untracked files
|⚑n     |there are `n` stashes on the repo

### Branch Tracking Symbols

Symbol  |Meaning
------- |-------
↑·n     |ahead of remote by `n` commits
↓·n     |behind remote by `n` commits
↓·m↑·n  |branches diverged, other by `m` commits, yours by `n` commits


### Branch Symbol

When the branch name starts with a colon `:`, it means it’s actually a hash, not a branch.
It should be pretty clear, unless you name your branches like hashes :-)

## Install

1.  Clone this repository somewhere on your hard drive.
1.  Source the file `zshrc.sh` from your `~/.zshrc` config file, and
    configure your prompt. So, somewhere in `~/.zshrc`, you should have:

    ```sh
    source path/to/zshrc.sh
    # an example prompt
    PROMPT='%B%m%~%b$(git_super_status) %# '
    ```

1.  Go in a git repository and test it!

### Haskell (optional)

There is now a Haskell implementation as well, which can be four to six times faster than the Python one. The reason is not that Haskell is faster in itself (although it is), but that this implementation calls `git` only once. To install, do the following:

1.  Make sure [Haskell's stack](http://docs.haskellstack.org/en/stable/README.html#how-to-install) is installed on your system
1.  `cd` to this folder
1.  Run `stack setup` to install the Haskell compiler, if it is not already there
1.  Run `stack build && stack install` (don't worry, the executable is only “installed” in this folder, not on your system)
1.  Define the variable `GIT_PROMPT_EXECUTABLE="haskell"` somewhere in your `.zshrc`

## Customization

- Define the variable `ZSH_THEME_GIT_PROMPT_CACHE=1` in order to enable caching.

- Define the variable `ZSH_GIT_PROMPT_SHOW_UPSTREAM=1` in order to see the remote branch you are tracking.

- Define the variable `ZSH_GIT_PROMPT_SHOW_UPSTREAM=2` to show the remote as above but omit the remote branch when its name is equal to the local branch.

Demo:

![upstream example](https://user-images.githubusercontent.com/470400/40869339-52ae782c-65e7-11e8-89a9-2e053b3f8198.png)

- By default, python version invokes `python`. To force a specific python interpreter: `ZSH_GIT_PROMPT_PYBIN=/usr/bin/python2.7`.

- You may redefine the function `git_super_status` (after the `source` statement) to adapt it to your needs (to change the order in which the information is displayed).

- To modify the symbols/colors of the theme, simply redefine the variables at bottom of the
the `zshrc.sh` after sourcing. This could be in your `~/.zshrc` or sourced elsewhere.
These are the defaults:

```sh
    ZSH_THEME_GIT_PROMPT_PREFIX="["
    ZSH_THEME_GIT_PROMPT_SUFFIX="]"
    ZSH_THEME_GIT_PROMPT_HASH_PREFIX=":"
    ZSH_THEME_GIT_PROMPT_SEPARATOR="|"
    ZSH_THEME_GIT_PROMPT_BRANCH="%{$fg_bold[magenta]%}"
    ZSH_THEME_GIT_PROMPT_STAGED="%{$fg[red]%}%{●%G%}"
    ZSH_THEME_GIT_PROMPT_CONFLICTS="%{$fg[red]%}%{✖%G%}"
    ZSH_THEME_GIT_PROMPT_CHANGED="%{$fg[blue]%}%{✚%G%}"
    ZSH_THEME_GIT_PROMPT_BEHIND="%{↓·%2G%}"
    ZSH_THEME_GIT_PROMPT_AHEAD="%{↑·%2G%}"
    ZSH_THEME_GIT_PROMPT_STASHED="%{$fg_bold[blue]%}%{⚑%G%}"
    ZSH_THEME_GIT_PROMPT_UNTRACKED="%{$fg[cyan]%}%{…%G%}"
    ZSH_THEME_GIT_PROMPT_CLEAN="%{$fg_bold[green]%}%{✔%G%}"
    ZSH_THEME_GIT_PROMPT_LOCAL=" L"
    # The remote branch will be shown between these two
    ZSH_THEME_GIT_PROMPT_UPSTREAM_FRONT=" {%{$fg_bold[blue]%}"
    ZSH_THEME_GIT_PROMPT_UPSTREAM_END="%{${reset_color}%}}"
```

**Enjoy!**

[blog post]: http://sebastiancelis.com/2009/nov/16/zsh-prompt-git-users/
