autoload -U colors && colors
setopt PROMPT_SUBST
autoload -U add-zsh-hook
add-zsh-hook chpwd   -chpwd-update-git-vars
add-zsh-hook precmd  -precmd-update-git-vars

-precmd-update-git-vars() {
  -update-current-git-vars
}

-chpwd-update-git-vars() {
  -update-current-git-vars
}

-update-current-git-vars() {
  if [[ "$(git rev-parse --is-inside-work-tree 2>/dev/null)" != "true" ]]; then
    return 0
  fi

  unset _ZSH_GP_PROMPT

  local git_branch
  local git_status
  local prefix="("
  local suffix=")"
  local seperator="|"
  local clean="clean"
  local yellow="%{$fg_bold[yellow]%}"
  local green="%{$fg_bold[green]%}"
  local red="%{$fg_bold[red]%}"
  local reset="%{${reset_color}%}"

  git_branch="$(git rev-parse --abbrev-ref=loose HEAD)"
  git_status="$(
    git status --porcelain \
    | awk '{print $1}' \
    | sort | uniq -c \
    | awk '{printf "%s", $2$1} END {printf "\n"}'
  )"

  _ZSH_GP_PROMPT="${prefix}${yellow}${git_branch}${reset}${seperator}"
  if [[ -n "$git_status" ]]; then
    _ZSH_GP_PROMPT="${_ZSH_GP_PROMPT}${red}${git_status}${reset}${suffix}"
  else
    _ZSH_GP_PROMPT="${_ZSH_GP_PROMPT}${green}${clean}${reset}${suffix}"
  fi
}


-git-status() {
  -precmd-update-git-vars

  if [[ -n "$_ZSH_GP_PROMPT" ]]; then
    echo "$_ZSH_GP_PROMPT"
  fi
}

## Local Variables:
# mode: Shell-Script
# sh-indentation: 2
# indent-tabs-mode: nil
# sh-basic-offset: 2
# End:
# vim: ft=zsh sw=2 ts=2 et
