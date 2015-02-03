module Utils where

import Data.Maybe (fromMaybe)
import Control.Applicative ((<$>), (<*>))
import BranchParse (Branch, BranchInfo(MkBranchInfo), branchInfo, Distance, pairFromDistance)
import StatusParse (Status(MakeStatus), processStatus)

{- Type aliases -}

newtype Hash = MkHash {getHash :: String}
type Numbers = [String]

{- Combining branch and status parsing -}

rightOrNothing :: Either a b -> Maybe b
rightOrNothing = either (const Nothing) Just

processBranch :: String -> Maybe BranchInfo
processBranch = rightOrNothing . branchInfo

processGitStatus :: [String] -> Maybe (BranchInfo, Status Int)
processGitStatus [] = Nothing
processGitStatus (branchLine:statusLines) = (,) <$> processBranch branchLine <*> processStatus statusLines

showStatusNumbers :: Status Int -> Numbers
showStatusNumbers (MakeStatus s x c t) = show <$> [s, x, c, t]


showBranchNumbers :: Maybe Distance -> Numbers
showBranchNumbers distance = show <$> [ahead, behind]
	where
		(ahead, behind) = fromMaybe (0,0)  -- the script needs some value, (0,0) means no display
			$ pairFromDistance <$> distance

{- Combine status info, branch info and hash -}

branchOrHashWith :: Char -> Maybe Hash -> Maybe Branch -> String
branchOrHashWith _ _ (Just branch) = show branch
branchOrHashWith c (Just hash) Nothing = c : getHash hash
branchOrHashWith _ Nothing _ = ""

allStrings :: Maybe Hash
			-> (BranchInfo, Status Int) 
			-> [String]
allStrings mhash (MkBranchInfo branch _ behead, stat) = branchOrHashWith ':' mhash branch : (showBranchNumbers behead ++ showStatusNumbers stat)

stringsFromStatus :: Maybe Hash
					-> String -- status
					-> Maybe [String]
stringsFromStatus h = fmap  (allStrings h) . processGitStatus . lines


