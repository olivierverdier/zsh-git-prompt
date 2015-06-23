# heavily inspired by http://qiita.com/mollifier/items/8d5a627d773758dd8078

setopt prompt_subst

autoload -Uz vcs_info
autoload -Uz add-zsh-hook
autoload -Uz is-at-least
autoload -Uz colors

zstyle ':vcs_info:*' max-exports 1
zstyle ':vcs_info:*' enable git

if is-at-least 4.3.10; then
  zstyle ':vcs_info:git:*' formats '%m'
  zstyle ':vcs_info:git:*' check-for-changes true
fi

if is-at-least 4.3.11; then
  zstyle ':vcs_info:git+set-message:*' hooks \
    git-hook-begin     \
    git-branch-name    \
    git-local-diff     \
    git-remote-diff    \
    git-stash-count

  # initial hook function
  # call this function only when inside git work tree
  function +vi-git-hook-begin() {
    if [[ "$(git rev-parse --is-inside-work-tree 2>/dev/null)" != 'true' ]]; then
      # if returing ret val 0, then do not call after function(s)
      return 1
    fi

    return 0
  }

  # more deeply insight git branch name than %b
  function +vi-git-branch-name() {
    local git_branch_name
    local yellow="%{$fg_bold[yellow]%}"
    local reset="%{${reset_color}%}"

    git_branch_name="$(git rev-parse --abbrev-ref=loose HEAD 2>/dev/null)"
    hook_com[misc]+="${yellow}${git_branch_name}${reset}|"
  }

  # display local diff(s)
  function +vi-git-local-diff() {

    local git_local_diff
    local clean="clean"
    local green="%{$fg_bold[green]%}"
    local red="%{$fg_bold[red]%}"
    local reset="%{${reset_color}%}"

    git_local_diff="$(
      git status --porcelain \
      | awk '{print $1}' \
      | sort | uniq -c \
      | awk '{printf "%s", $2$1} END {printf "\n"}'
    )"

    if [[ -n "$git_local_diff" ]]; then
      hook_com[misc]+="${red}$git_local_diff${reset}"
    else
      hook_com[misc]+="${green}${clean}${reset}"
    fi
  }

  # display remote diff(s)
  function +vi-git-remote-diff() {

    local git_remote_name
    local git_branch_name
    local git_remote_diff
    local red="%{$fg_bold[red]%}"
    local reset="%{${reset_color}%}"

    git_branch_name="$(git rev-parse --abbrev-ref=loose HEAD 2>/dev/null)"
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

    if [[ -n "$git_remote_diff" ]]; then
      hook_com[misc]+="|${red}${git_remote_diff}${reset}"
    fi
  }

  # display stashed number(s)
  function +vi-git-stash-count() {

    local stash_num
    local stash_suffix="S"
    local red="%{$fg_bold[red]%}"
    local reset="%{${reset_color}%}"

    stash_num=$(git stash list 2>/dev/null | wc -l | tr -d ' ')

    if [[ $stash_num -ne 0 ]]; then
      hook_com[misc]+="|${red}${stash_suffix}${stash_num}${reset}"
    fi
  }
fi

function -zsh-git-prompt() {
  local -a messages
  local zsh_git_prompt
  LANG=en_US.UTF-8 vcs_info

  # if vcs_info is empty, then not set prompt
  if [[ -z "$vcs_info_msg_0_" ]]; then
    zsh_git_prompt=""
  else
    messages=("(")
    [[ -n "$vcs_info_msg_0_" ]] && messages+=("$vcs_info_msg_0_")
    messages+=(")")

    # concatenate
    zsh_git_prompt="${(j::)messages}"
  fi

  #RPROMPT="$prompt"
  #PROMPT='%n %F{blue}%~%f%b '
  #PROMPT="${PROMPT}${_ZSH_GIT_PROMPT}"
  #PROMPT="${PROMPT}"'[%?]'$'\n''%(!,#,$) '
  #PROMPT='%n %F{blue}%~%f%b' $_ZSH_GIT_PROMPT'[%?]'$'\n''%(!,#,$) '
  echo "$zsh_git_prompt"
}

# Local Variables:
# mode: Shell-Script
# sh-indentation: 2
# indent-tabs-mode: nil
# sh-basic-offset: 2
# End:
# vim: ft=zsh sw=2 ts=2 et
