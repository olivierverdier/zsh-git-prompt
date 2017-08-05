module Utils where

import Data.Maybe (fromMaybe)
import BranchParse (Branch(MkBranch), MBranchInfo, BranchInfo(MkBranchInfo), branchInfo, getDistance, pairFromDistance, Remote)
import StatusParse (Status(MakeStatus), processStatus)

{- Type aliases -}

newtype Hash = MkHash {getHash :: String}

newtype Stash = MkStash {getStash :: Int}

data GitInfo = MkGitInfo MBranchInfo (Status Int) Stash

{- Combining branch and status parsing -}

rightOrNothing :: Either a b -> Maybe b
rightOrNothing = either (const Nothing) Just

processBranch :: String -> Maybe MBranchInfo
processBranch = rightOrNothing . branchInfo

processGitStatus :: [String] -> Stash -> Maybe GitInfo
processGitStatus [] _ = Nothing
processGitStatus (branchLine:statusLines) stash =
		do -- Maybe
			mbranch <- processBranch branchLine
			status <- processStatus statusLines
			return (MkGitInfo mbranch status stash)

showStatusNumbers :: Status Int -> [String]
showStatusNumbers (MakeStatus s x c t) =
		do -- List
			nb <- [s, x, c, t]
			return (show nb)

showRemoteNumbers :: Maybe Remote -> [String]
showRemoteNumbers mremote =
		do -- List
			ab <- [ahead, behind]
			return (show ab)
	where
		(ahead, behind) = fromMaybe (0,0) distance  -- the script needs some value, (0,0) means no display
		distance = do -- Maybe
			remote <- mremote
			dist <- getDistance remote
			return (pairFromDistance dist)

showBranchInfo :: BranchInfo -> [String]
showBranchInfo (MkBranchInfo branch mremote) = show branch : showRemoteNumbers mremote

{- Combine status info, branch info and hash -}

branchOrHashWith :: Char -> Maybe Hash -> Maybe BranchInfo -> BranchInfo
branchOrHashWith _ _ (Just bi) = bi
branchOrHashWith c (Just hash) Nothing = MkBranchInfo (MkBranch (c : getHash hash)) Nothing
branchOrHashWith _ Nothing _ = MkBranchInfo (MkBranch "") Nothing

showGitInfo :: Maybe Hash
			-> GitInfo
			-> [String]
showGitInfo mhash (MkGitInfo bi stat stash) =
	branchInfoString ++ showStatusNumbers stat ++ [show (getStash stash)]
	where
		branchInfoString = showBranchInfo (branchOrHashWith ':' mhash bi)

stringsFromStatus :: Maybe Hash
					-> String -- status
					-> Stash -- stash
					-> Maybe [String]
stringsFromStatus h status stash = do -- List
		processed <- processGitStatus (lines status) stash
		return (showGitInfo h processed)
