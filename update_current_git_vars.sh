unset __CURRENT_GIT_BRANCH
unset __CURRENT_GIT_BRANCH_STATUS
unset __CURRENT_GIT_BRANCH_IS_DIRTY
local st="$(git status 2>/dev/null)"
if [ -n "$st" ]; then
	local -a arr
	arr=(${(f)st})
	if echo $arr[1] | grep "Not currently on any branch." >/dev/null; then
		__CURRENT_GIT_BRANCH='no-branch'
	else
		__CURRENT_GIT_BRANCH="$(echo $arr[1] | awk ' { print $4 } ')"
	fi
	if echo $arr[2] | grep "Your branch is" >/dev/null; then
		if echo $arr[2] | grep "ahead" >/dev/null; then
			__CURRENT_GIT_BRANCH_STATUS='ahead'
		elif echo $arr[2] | grep "diverged" >/dev/null; then
			__CURRENT_GIT_BRANCH_STATUS='diverged'
		else
			__CURRENT_GIT_BRANCH_STATUS='behind'
		fi
	fi
	if echo $st | grep "nothing to commit (working directory clean)" >/dev/null; then
	else
		__CURRENT_GIT_BRANCH_IS_DIRTY='1'
	fi
fi