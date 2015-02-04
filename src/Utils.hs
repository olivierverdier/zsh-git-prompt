module Utils where

import Data.Maybe (fromMaybe)
import Control.Applicative ((<$>), (<*>))
import BranchParse (Branch(MkBranch), MBranchInfo, BranchInfo(MkBranchInfo), branchInfo, getDistance, pairFromDistance, Remote)
import StatusParse (Status(MakeStatus), processStatus)

{- Type aliases -}

newtype Hash = MkHash {getHash :: String}

data GitInfo = MkGitInfo MBranchInfo (Status Int)

{- Combining branch and status parsing -}

rightOrNothing :: Either a b -> Maybe b
rightOrNothing = either (const Nothing) Just

processBranch :: String -> Maybe MBranchInfo
processBranch = rightOrNothing . branchInfo

processGitStatus :: [String] -> Maybe GitInfo
processGitStatus [] = Nothing
processGitStatus (branchLine:statusLines) = MkGitInfo <$> processBranch branchLine <*> processStatus statusLines

showStatusNumbers :: Status Int -> [String]
showStatusNumbers (MakeStatus s x c t) = show <$> [s, x, c, t]


showRemoteNumbers :: Maybe Remote -> [String]
showRemoteNumbers mremote = show <$> [ahead, behind]
	where
		(ahead, behind) = fromMaybe (0,0)  -- the script needs some value, (0,0) means no display
			$ pairFromDistance <$> (getDistance =<< mremote)

showBranchInfo :: BranchInfo -> [String]
showBranchInfo (MkBranchInfo branch mremote) = show branch : showRemoteNumbers mremote

{- Combine status info, branch info and hash -}

branchOrHashWith :: Char -> Maybe Hash -> Maybe BranchInfo -> [String]
branchOrHashWith _ _ (Just bi) = showBranchInfo bi
branchOrHashWith c (Just hash) Nothing = showBranchInfo $ MkBranchInfo (MkBranch $ c : getHash hash) Nothing
branchOrHashWith _ Nothing _ = showBranchInfo $ MkBranchInfo (MkBranch "") Nothing

allStrings :: Maybe Hash
			-> GitInfo 
			-> [String]
allStrings mhash (MkGitInfo bi stat) = branchOrHashWith ':' mhash bi ++  showStatusNumbers stat

stringsFromStatus :: Maybe Hash
					-> String -- status
					-> Maybe [String]
stringsFromStatus h = fmap  (allStrings h) . processGitStatus . lines


