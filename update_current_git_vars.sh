unset __CURRENT_GIT_STATUS

local gitstatus="${HOME}/.zsh/gitstatus.py"
_GIT_STATUS=`python ${gitstatus}`
__CURRENT_GIT_STATUS=(${(s/:/)_GIT_STATUS})
