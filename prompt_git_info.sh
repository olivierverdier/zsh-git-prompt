if [ -n "$__CURRENT_GIT_BRANCH" ]; then
	local s="("
	s+="$__CURRENT_GIT_BRANCH"
	case "$__CURRENT_GIT_BRANCH_STATUS" in
		ahead)
		s+="↑"
		;;
		diverged)
		s+="↕"
		;;
		behind)
		s+="↓"
		;;
	esac
	if [ -n "$__CURRENT_GIT_BRANCH_IS_DIRTY" ]; then
		s+="⚡"
	fi
	s+=")"
 
	printf " %s%s" "%{${fg[yellow]}%}" $s
fi