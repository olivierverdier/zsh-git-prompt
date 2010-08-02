# To install source this file from your .zshrc file

# Change this to reflect your installation directory
export __GIT_PROMPT_DIR=~/.zsh/git-prompt
# Initialize colors.
autoload -U colors
colors
 
# Allow for functions in the prompt.
setopt PROMPT_SUBST
 
# Autoload zsh functions.
fpath=($__GIT_PROMPT_DIR/functions $fpath)
autoload -U $__GIT_PROMPT_DIR/functions/*(:t)

function preexec() {
    preexec_update_git_vars
}

function precmd() {
    precmd_update_git_vars
}

function chpwd() {
    chpwd_update_git_vars
}
 

# Set the prompt.
PROMPT='%B%m%~%b$(prompt_git_info) %# '
# for a right prompt:
# RPROMPT='%b$(prompt_git_info)'
