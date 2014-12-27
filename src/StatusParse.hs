module StatusParse where

{- Full status information -}
data Status a = MakeStatus {
	staged :: a,
	conflict :: a,
	changed :: a,
	untracked :: a} deriving (Eq, Show)

{- The two characters starting a git status line: -}
type MiniStatus = (Char, Char)

{- Interpretation of mini status -}
isChanged :: MiniStatus -> Bool
isChanged (index,work) =
		work == 'M' || (work == 'D' && index /= 'D')

isStaged :: MiniStatus -> Bool
isStaged (index,work) =
		(index `elem` "MRC") || (index == 'D' && work /= 'D') || (index == 'A' && work /= 'A')

isConflict :: MiniStatus -> Bool
isConflict (index,work) =
		index == 'U' || work == 'U' || (index == 'A' && work == 'A') || (index == 'D' && work == 'D')

isUntracked :: MiniStatus -> Bool
isUntracked (index,_) =
		index == '?'

countByType :: (MiniStatus -> Bool) -> [MiniStatus] -> Int
countByType isType = length . filter isType

countStatus :: [MiniStatus] -> Status Int
countStatus l = MakeStatus 
	{
 	staged=countByType isStaged l,
	conflict=countByType isConflict l,
	changed=countByType isChanged l,
	untracked=countByType isUntracked l
	}

extractMiniStatus :: String -> Maybe MiniStatus
extractMiniStatus [] = Nothing
extractMiniStatus [_] = Nothing
extractMiniStatus (index:work:_) = Just (index,work)

processStatus :: [String] -> Maybe (Status Int)
processStatus = fmap countStatus . sequence . fmap extractMiniStatus

