import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(ExitSuccess))
import System.IO.Unsafe (unsafeInterleaveIO)
import Data.List (intercalate)

import Utils (stringsFromStatus, Hash(MkHash))
import Data.Maybe (fromMaybe)
import Control.Applicative ((<$>))

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

{- main -}

main :: IO ()
main = do
	mstatus <- gitstatus
	mhash <- unsafeInterleaveIO gitrevparse -- defer the execution until we know we need the hash
	let result = do
		status <- mstatus
		strings <- stringsFromStatus mhash status
		return $ intercalate " " strings
	putStr $ fromMaybe "" result
