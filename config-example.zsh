## Local configuration file for zsh git prompt
## Rename to "config.zsh" and customize to taste

## Default values for the appearance of the prompt
ZSH_THEME_GIT_PROMPT_PREFIX="("
ZSH_THEME_GIT_PROMPT_SUFFIX=")"
ZSH_THEME_GIT_PROMPT_SEPARATOR="|"
ZSH_THEME_GIT_PROMPT_BRANCH="%{$fg_bold[magenta]%}"
ZSH_THEME_GIT_PROMPT_STAGED="%{$fg[red]%}●"
ZSH_THEME_GIT_PROMPT_CONFLICTS="%{$fg[red]%}✖"
ZSH_THEME_GIT_PROMPT_CHANGED="%{$fg[blue]%}✚"
ZSH_THEME_GIT_PROMPT_REMOTE=""
ZSH_THEME_GIT_PROMPT_UNTRACKED="…"
ZSH_THEME_GIT_PROMPT_CLEAN="%{$fg_bold[green]%}✔"

ZSH_THEME_GIT_PROMPT_TAG_PREFIX="("
ZSH_THEME_GIT_PROMPT_TAG_SUFFIX=")"

# Always show tag information?
# 0 - Only show the tag when it points directly to HEAD
# 1 - Always show the tag (find the "parent tag" if necessary)
ZSH_GIT_PROMPT_ALWAYS_SHOW_TAG=1
