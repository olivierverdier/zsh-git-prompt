module StatusParse (processStatus, StatusT(StatusC, staged, conflict, changed, untracked)) where

{- Full status information -}
data StatusT a = StatusC {
	staged :: a,
	conflict :: a,
	changed :: a,
	untracked :: a} deriving (Eq, Show)

{- The two characters starting a git status line: -}
type MiniStatus = (Char, Char)

{- Interpretation of mini status -}
isChanged :: MiniStatus -> Bool
isChanged (i,w) =
		w == 'M' || (w == 'D' && i /= 'D')

isStaged :: MiniStatus -> Bool
isStaged (i,w) =
		(i `elem` "MRC") || (i == 'D' && w /= 'D') || (i == 'A' && w /= 'A')

isConflict :: MiniStatus -> Bool
isConflict (i,w) =
		i == 'U' || w == 'U' || (i == 'A' && w == 'A') || (i == 'D' && w == 'D')

isUntracked :: MiniStatus -> Bool
isUntracked (i,_) =
		i == '?'

countByType :: (MiniStatus -> Bool) -> [MiniStatus] -> Int
countByType isType = length . filter isType

countStatus :: [MiniStatus] -> StatusT Int
countStatus l = StatusC 
	{
 	staged=countByType isStaged l,
	conflict=countByType isConflict l,
	changed=countByType isChanged l,
	untracked=countByType isUntracked l
	}

extractMiniStatus :: String -> MiniStatus
extractMiniStatus [] = undefined
extractMiniStatus [_] = undefined
extractMiniStatus (i:w:_) = (i,w)

processStatus :: [String] -> StatusT Int
processStatus = countStatus . fmap extractMiniStatus

