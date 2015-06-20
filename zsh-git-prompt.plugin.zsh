export _GIT_PROMPT_DIR=${0:A:h}
export _GIT_PROMPT_EXECUTABLE=${_GIT_PROMPT_USE_PYTHON:-"python"}

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
    _EXECUTED_GIT_COMMAND=1
    ;;
  esac
}

-precmd-update-git-vars() {
  if [[ -n "$_EXECUTED_GIT_COMMAND" ]]; then
    -update-current-git-vars
    unset _EXECUTED_GIT_COMMAND
  fi
}

-chpwd-update-git-vars() {
  -update-current-git-vars
}

-update-current-git-vars() {
  unset _CURRENT_GIT_STATUS

  if [[ "$_GIT_PROMPT_EXECUTABLE" == "python" ]]; then
      local gitstatus="${_GIT_PROMPT_DIR}/gitstatus.py"
      _GIT_STATUS=`python ${gitstatus} 2>/dev/null`
  fi

  _CURRENT_GIT_STATUS=("${(@s: :)_GIT_STATUS}")
  _GIT_BRANCH=$_CURRENT_GIT_STATUS[1]
  _GIT_AHEAD=$_CURRENT_GIT_STATUS[2]
  _GIT_BEHIND=$_CURRENT_GIT_STATUS[3]
  _GIT_STAGED=$_CURRENT_GIT_STATUS[4]
  _GIT_CONFLICTS=$_CURRENT_GIT_STATUS[5]
  _GIT_CHANGED=$_CURRENT_GIT_STATUS[6]
  _GIT_UNTRACKED=$_CURRENT_GIT_STATUS[7]
}


-git-super-status() {
  -precmd-update-git-vars
  if [[ -n "$_CURRENT_GIT_STATUS" ]]; then
    _STATUS="${_ZSH_GP_PREFIX}${_ZSH_GP_YELLOW}${_GIT_BRANCH}%{${reset_color}%}"
    if [[ $_GIT_BEHIND -ne 0 ]]; then
      _STATUS="${_STATUS}${_ZSH_GP_BASIC_COLOR}${_ZSH_GP_BEHIND}${_GIT_BEHIND}%{${reset_color}%}"
    fi
    if [[ $_GIT_AHEAD -ne 0 ]]; then
      _STATUS="${_STATUS}${_ZSH_GP_BASIC_COLOR}${_ZSH_GP_AHEAD}${_GIT_AHEAD}%{${reset_color}%}"
    fi
    _STATUS="${_STATUS}${_ZSH_GP_SEPARATOR}"
    if [[ $_GIT_STAGED -ne 0 ]]; then
      _STATUS="${_STATUS}${_ZSH_GP_BASIC_COLOR}${_ZSH_GP_STAGED}${_GIT_STAGED}%{${reset_color}%}"
    fi
    if [[ $_GIT_CONFLICTS -ne 0 ]]; then
      _STATUS="${_STATUS}${_ZSH_GP_BASIC_COLOR}${_ZSH_GP_CONFLICTS}${_GIT_CONFLICTS}%{${reset_color}%}"
    fi
    if [[ $_GIT_CHANGED -ne 0 ]]; then
      _STATUS="${_STATUS}${_ZSH_GP_BASIC_COLOR}${_ZSH_GP_CHANGED}${_GIT_CHANGED}%{${reset_color}%}"
    fi
    if [[ $_GIT_UNTRACKED -ne 0 ]]; then
      _STATUS="${_STATUS}${_ZSH_GP_BASIC_COLOR}${_ZSH_GP_UNTRACKED}${_GIT_UNTRACKED}%{${reset_color}%}"
    fi
    if [[ $_GIT_CHANGED -eq 0 && $_GIT_CONFLICTS -eq 0 && $_GIT_STAGED -eq 0 && $_GIT_UNTRACKED -eq 0 ]]; then
      _STATUS="${_STATUS}${_ZSH_GP_GREEN}${_ZSH_GP_CLEAN}"
    fi
    _STATUS="${_STATUS}%{${reset_color}%}${_ZSH_GP_SUFFIX}"
    echo "$_STATUS"
  fi
}

# Default values for the appearance of the prompt. Configure at will.
_ZSH_GP_PREFIX="("
_ZSH_GP_SUFFIX=")"
_ZSH_GP_SEPARATOR="|"
_ZSH_GP_BASIC_COLOR="%{$fg_bold[red]%}%{%G%}"
_ZSH_GP_YELLOW="%{$fg_bold[yellow]%}"
_ZSH_GP_GREEN="%{$fg_bold[green]%}%{%G%}"
_ZSH_GP_STAGED="A"
_ZSH_GP_CONFLICTS="UU"
_ZSH_GP_CHANGED="M"
_ZSH_GP_AHEAD=">"
_ZSH_GP_BEHIND="<"
_ZSH_GP_UNTRACKED="??"
_ZSH_GP_CLEAN="clean"

# Local Variables:
# mode: Shell-Script
# sh-indentation: 2
# indent-tabs-mode: nil
# sh-basic-offset: 2
# End:
# vim: ft=zsh sw=2 ts=2 et
