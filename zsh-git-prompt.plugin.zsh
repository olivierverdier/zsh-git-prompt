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

  local git_branch_name
  local git_status
  local git_remote_name
  local git_remote_diff
  local prefix="("
  local suffix=")"
  local seperator="|"
  local clean="clean"
  local yellow="%{$fg_bold[yellow]%}"
  local green="%{$fg_bold[green]%}"
  local red="%{$fg_bold[red]%}"
  local reset="%{${reset_color}%}"

  git_branch_name="$(git rev-parse --abbrev-ref=loose HEAD 2>/dev/null)"
  git_status="$(
    git status --porcelain \
    | awk '{print $1}' \
    | sort | uniq -c \
    | awk '{printf "%s", $2$1} END {printf "\n"}'
  )"

  git_remote_name="$(git config branch.${git_branch_name}.remote 2>/dev/null)"
  if [[ -n "$git_remote_name" ]]; then
    git_remote_diff="$(
      git rev-list --left-right \
        refs/remotes/${git_remote_name}/${git_branch_name}...HEAD \
        | grep -oE '^[><]' \
        | sort | uniq -c \
        | awk '{printf "%s", $2$1} END {printf "\n"}'
    )"
  fi

  _ZSH_GP_PROMPT="${prefix}${yellow}${git_branch_name}${reset}${seperator}"
  if [[ -z "$git_status" ]]; then
    _ZSH_GP_PROMPT="${_ZSH_GP_PROMPT}${green}${clean}${reset}${suffix}"
  else
    _ZSH_GP_PROMPT="${_ZSH_GP_PROMPT}${red}${git_status}${reset}"
    if [[ -n "$git_remote_diff" ]]; then
      _ZSH_GP_PROMPT="${_ZSH_GP_PROMPT}${seperator}${red}${git_remote_diff}${reset}"
    fi
    _ZSH_GP_PROMPT="${_ZSH_GP_PROMPT}${suffix}"
  fi
}


-git-status() {
  unset _ZSH_GP_PROMPT
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
