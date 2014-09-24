import System.Process
import System.Exit
import Control.Applicative
import Data.Maybe
import BranchParse
import StatusParse

{- Type aliases -}

type Hash = String
type Numbers = [String]

{- Combining branch and status parsing -}

processBranch :: String -> BranchInfo
processBranch = either (const noBranchInfo) id . branchInfo . drop 3

processGitStatus :: [String] -> (BranchInfo, StatusT Int)
processGitStatus [] = undefined
processGitStatus (branchLine:statusLines) = (processBranch branchLine, processStatus statusLines)

allInfo :: (BranchInfo, StatusT Int) -> (Maybe Branch, Numbers)
allInfo (((branch, _), behead), StatusC s x c t) = (branch , fmap show [ahead, behind, s, x, c, t])
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


printAll :: Maybe String -> IO ()
printAll status = 
	case fmap (allInfo . processGitStatus . lines) status of
		Nothing -> return ()
		Just (branch,numbers) -> printBranch branch >> printNumbers numbers
	-- maybe (return ()) (\(b,n) -> printBranch b >> printNumbers n) $ fmap (allInfo . processGitStatus . lines) status

-- main

main :: IO ()
main = gitstatus >>= printAll
