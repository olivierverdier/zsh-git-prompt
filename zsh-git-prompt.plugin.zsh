export __GIT_PROMPT_DIR=${0:A:h}
export GIT_PROMPT_EXECUTABLE=${GIT_PROMPT_USE_PYTHON:-"python"}

# Initialize colors.
autoload -U colors && colors

# Allow for functions in the prompt.
setopt PROMPT_SUBST

autoload -U add-zsh-hook
add-zsh-hook chpwd   -chpwd-update-git-vars
add-zsh-hook preexec -preexec-update-git-vars
add-zsh-hook precmd  -precmd-update-git-vars

## Function definitions
-preexec-update-git-vars() {
  case "$2" in
    git*|hub*|gh*|stg*)
    __EXECUTED_GIT_COMMAND=1
    ;;
  esac
}

-precmd-update-git-vars() {
  if [[ -n "$__EXECUTED_GIT_COMMAND" || -z "$ZSH_GP_CACHE" ]]; then
    -update-current-git-vars
    unset __EXECUTED_GIT_COMMAND
  fi
}

-chpwd-update-git-vars() {
  -update-current-git-vars
}

-update-current-git-vars() {
  unset __CURRENT_GIT_STATUS

  if [[ "$GIT_PROMPT_EXECUTABLE" == "python" ]]; then
      local gitstatus="${__GIT_PROMPT_DIR}/gitstatus.py"
      _GIT_STATUS=`python ${gitstatus} 2>/dev/null`
  fi

  __CURRENT_GIT_STATUS=("${(@s: :)_GIT_STATUS}")
  GIT_BRANCH=$__CURRENT_GIT_STATUS[1]
  GIT_AHEAD=$__CURRENT_GIT_STATUS[2]
  GIT_BEHIND=$__CURRENT_GIT_STATUS[3]
  GIT_STAGED=$__CURRENT_GIT_STATUS[4]
  GIT_CONFLICTS=$__CURRENT_GIT_STATUS[5]
  GIT_CHANGED=$__CURRENT_GIT_STATUS[6]
  GIT_UNTRACKED=$__CURRENT_GIT_STATUS[7]
}


-git-super-status() {
  -precmd-update-git-vars
  if [[ -n "$__CURRENT_GIT_STATUS" ]]; then
    STATUS="${ZSH_GP_PREFIX}${ZSH_GP_YELLOW}${GIT_BRANCH}%{${reset_color}%}"
    if [[ $GIT_BEHIND -ne 0 ]]; then
      STATUS="${STATUS}${ZSH_GP_BASIC_COLOR}${ZSH_GP_BEHIND}${GIT_BEHIND}%{${reset_color}%}"
    fi
    if [[ $GIT_AHEAD -ne 0 ]]; then
      STATUS="${STATUS}${ZSH_GP_BASIC_COLOR}${ZSH_GP_AHEAD}${GIT_AHEAD}%{${reset_color}%}"
    fi
    STATUS="${STATUS}${ZSH_GP_SEPARATOR}"
    if [[ $GIT_STAGED -ne 0 ]]; then
      STATUS="${STATUS}${ZSH_GP_BASIC_COLOR}${ZSH_GP_STAGED}${GIT_STAGED}%{${reset_color}%}"
    fi
    if [[ $GIT_CONFLICTS -ne 0 ]]; then
      STATUS="${STATUS}${ZSH_GP_BASIC_COLOR}${ZSH_GP_CONFLICTS}${GIT_CONFLICTS}%{${reset_color}%}"
    fi
    if [[ $GIT_CHANGED -ne 0 ]]; then
      STATUS="${STATUS}${ZSH_GP_BASIC_COLOR}${ZSH_GP_CHANGED}${GIT_CHANGED}%{${reset_color}%}"
    fi
    if [[ $GIT_UNTRACKED -ne 0 ]]; then
      STATUS="${STATUS}${ZSH_GP_BASIC_COLOR}${ZSH_GP_UNTRACKED}${GIT_UNTRACKED}%{${reset_color}%}"
    fi
    if [[ $GIT_CHANGED -eq 0 && $GIT_CONFLICTS -eq 0 && $GIT_STAGED -eq 0 && $GIT_UNTRACKED -eq 0 ]]; then
      STATUS="${STATUS}${ZSH_GP_GREEN}${ZSH_GP_CLEAN}"
    fi
    STATUS="${STATUS}%{${reset_color}%}${ZSH_GP_SUFFIX}"
    echo "$STATUS"
  fi
}

# Default values for the appearance of the prompt. Configure at will.
ZSH_GP_PREFIX="("
ZSH_GP_SUFFIX=")"
ZSH_GP_SEPARATOR="|"
ZSH_GP_BASIC_COLOR="%{$fg[red]%}%{%G%}"
ZSH_GP_YELLOW="%{$fg_bold[yellow]%}"
ZSH_GP_GREEN="%{$fg_bold[green]%}%{%G%}"
ZSH_GP_STAGED="A"
ZSH_GP_CONFLICTS="UU"
ZSH_GP_CHANGED="M"
ZSH_GP_AHEAD=">"
ZSH_GP_BEHIND="<"
ZSH_GP_UNTRACKED="??"
ZSH_GP_CLEAN="clean"

# Local Variables:
# mode: Shell-Script
# sh-indentation: 2
# indent-tabs-mode: nil
# sh-basic-offset: 2
# End:
# vim: ft=zsh sw=2 ts=2 et
