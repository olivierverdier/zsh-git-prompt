unset __CURRENT_GIT_STATUS

local gitstatus="$__GIT_PROMPT_DIR/gitstatus.py"
_GIT_STATUS=`python ${gitstatus}`
__CURRENT_GIT_STATUS=("${(f)_GIT_STATUS}")
