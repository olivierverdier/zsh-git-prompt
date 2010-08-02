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
 
# Enable auto-execution of functions.
typeset -ga preexec_functions
typeset -ga precmd_functions
typeset -ga chpwd_functions
 
# Append git functions needed for prompt.
preexec_functions+='preexec_update_git_vars'
precmd_functions+='precmd_update_git_vars'
chpwd_functions+='chpwd_update_git_vars'

# Set the prompt.
PROMPT='%B%m%~%b$(prompt_git_info) %# '
# for a right prompt:
# RPROMPT='%b$(prompt_git_info)'
