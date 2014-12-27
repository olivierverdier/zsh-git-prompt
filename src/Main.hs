import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(ExitSuccess))
import Data.Maybe (fromMaybe)
import Control.Applicative ((<$>), (<*>))
import BranchParse (Branch, BranchInfo, branchInfo, AheadBehind)
import StatusParse (Status(MakeStatus), processStatus)

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
processGitStatus (branchLine:statusLines) = (,) <$> (processBranch branchLine) <*> (processStatus statusLines)

showStatusNumbers :: Status Int -> Numbers
showStatusNumbers (MakeStatus s x c t) = show <$> [s, x, c, t]

showBranchNumbers :: Maybe AheadBehind -> Numbers
showBranchNumbers behead = show <$> [ahead, behind]
	where
		(ahead, behind) = fromMaybe (0,0) behead

makeHash :: Maybe Hash -> String
makeHash = (':' :) . maybe "" init 


{- Git commands -}

maybeResult :: (ExitCode, a, b) -> Maybe a
maybeResult (exitCode, output, _) = 
	if exitCode == ExitSuccess then Just output else Nothing

safeRun :: String -> [String] -> IO (Maybe String)
safeRun command arguments = maybeResult <$> readProcessWithExitCode command arguments ""

gitstatus :: IO (Maybe String)
gitstatus =   safeRun "git" ["status", "--porcelain", "--branch"]

gitrevparse :: IO (Maybe Hash)
gitrevparse = safeRun "git" ["rev-parse", "--short", "HEAD"]

{- IO -}

printHash :: IO ()
printHash = putStrLn . makeHash =<< gitrevparse

printBranch :: Maybe Branch -> IO ()
printBranch branch =
	case branch of
		Nothing -> printHash
		Just bn -> putStrLn bn

printNumbers :: Numbers -> IO ()
printNumbers = mapM_ putStrLn

allInfo :: (BranchInfo, Status Int) -> (Maybe Branch, Numbers)
allInfo (((branch, _), behead), stat) = (branch, showBranchNumbers behead ++ showStatusNumbers stat)

printAll :: Maybe String -> IO ()
printAll gitStatus =
	case fmap allInfo . processGitStatus . lines =<< gitStatus of
		Nothing -> return ()
		Just (branch,numbers) -> printBranch branch >> printNumbers numbers

{- main -}

main :: IO ()
main = gitstatus >>= printAll
