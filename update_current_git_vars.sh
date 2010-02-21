unset __CURRENT_GIT_BRANCH
local stat="$(git status 2>/dev/null)"

local gitstatus="${HOME}/.zsh/gitstatus.py"
if [ -n "$stat" ]; then
	__CURRENT_GIT_BRANCH=`python ${gitstatus}`
fi
