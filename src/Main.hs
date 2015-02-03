import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(ExitSuccess))
import Data.Maybe (fromMaybe)
import Control.Applicative ((<$>), (<*>))
import BranchParse (Branch, BranchInfo(MkBranchInfo), branchInfo, Distance, pairFromDistance)
import StatusParse (Status(MakeStatus), processStatus)
import Data.List (intercalate)
import System.IO.Unsafe (unsafeInterleaveIO)

{- Type aliases -}

newtype Hash = MkHash {getHash :: String}
type Numbers = [String]

{- Combining branch and status parsing -}

rightOrNothing :: Either a b -> Maybe b
rightOrNothing = either (const Nothing) Just

processBranch :: String -> Maybe BranchInfo
processBranch = rightOrNothing . branchInfo . drop 3

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

{- Git commands -}

successOrNothing :: (ExitCode, a, b) -> Maybe a
successOrNothing (exitCode, output, _) =
	if exitCode == ExitSuccess then Just output else Nothing

safeRun :: String -> [String] -> IO (Maybe String)
safeRun command arguments = successOrNothing <$> readProcessWithExitCode command arguments ""

gitstatus :: IO (Maybe String)
gitstatus =   safeRun "git" ["status", "--porcelain", "--branch"]

gitrevparse :: IO (Maybe Hash)
gitrevparse = do
		result <- safeRun "git" ["rev-parse", "--short", "HEAD"]
		return $ MkHash . init <$> result

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


{- main -}

main :: IO ()
main = do
	mstatus <- gitstatus
	mhash <- unsafeInterleaveIO gitrevparse -- defer the execution until we know we need the hash
	let result = do
		status <- mstatus
		strings <- stringsFromStatus mhash status
		return $ intercalate " " strings
	putStrLn $ fromMaybe "" result
