import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(ExitSuccess))
import Data.Maybe (fromMaybe)
import Control.Applicative ((<$>), (<*>))
import BranchParse (Branch, BranchInfo, branchInfo, Distance, pairFromDistance)
import StatusParse (Status(MakeStatus), processStatus)
import Data.List (intercalate)

{- Type aliases -}

type Hash = String
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
		(ahead, behind) = fromMaybe (0,0) $ pairFromDistance <$> distance -- the script needs some value, (0,0) means no display

makeHashWith :: Char -- prefix to hashes
				-> Maybe Hash
				-> String
makeHashWith _ Nothing = "" -- some error in gitrevparse
makeHashWith _ (Just "") = "" -- hash too short
makeHashWith c (Just hash) = c : init hash

{- Git commands -}

successOrNothing :: (ExitCode, a, b) -> Maybe a
successOrNothing (exitCode, output, _) =
	if exitCode == ExitSuccess then Just output else Nothing

safeRun :: String -> [String] -> IO (Maybe String)
safeRun command arguments = successOrNothing <$> readProcessWithExitCode command arguments ""

gitstatus :: IO (Maybe String)
gitstatus =   safeRun "git" ["status", "--porcelain", "--branch"]

gitrevparse :: IO (Maybe Hash)
gitrevparse = safeRun "git" ["rev-parse", "--short", "HEAD"]

{- IO -}

branchOrHash :: Maybe Branch -> IO String
branchOrHash branch =
	case branch of
		Nothing -> makeHashWith ':' <$> gitrevparse
		Just bn -> return bn

allInfo :: (BranchInfo, Status Int) -> (IO String, Numbers)
allInfo (((branch, _), behead), stat) = (branchOrHash branch, showBranchNumbers behead ++ showStatusNumbers stat)

ioStrings :: (IO String, Numbers) -> IO [String]
ioStrings (ios,ss) = (: ss) <$> ios

stringsFromStatus :: String -> Maybe (IO [String])
stringsFromStatus = fmap  (ioStrings .  allInfo) . processGitStatus . lines

makeStringWith :: String -- string to intercalate with
				-> Maybe (IO [String])
				-> IO String
makeStringWith _ Nothing = return "" -- some parsing error
makeStringWith s (Just ios) = intercalate s <$> ios

stringFromStatus :: Maybe String -> IO String
stringFromStatus Nothing = return "" -- error in gitstatus
stringFromStatus (Just s) = makeStringWith " " . stringsFromStatus $ s

{- main -}

main :: IO ()
main = putStrLn =<< stringFromStatus =<< gitstatus
