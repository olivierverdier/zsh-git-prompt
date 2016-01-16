import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(ExitSuccess))
import System.IO.Unsafe (unsafeInterleaveIO)

import Utils (stringsFromStatus, Hash(MkHash))
import Data.Maybe (fromMaybe)

{- Git commands -}

successOrNothing :: (ExitCode, a, b) -> Maybe a
successOrNothing (exitCode, output, _) =
	if exitCode == ExitSuccess then Just output else Nothing

safeRun :: String -> [String] -> IO (Maybe String)
safeRun command arguments =
	do -- IO
		output <- readProcessWithExitCode command arguments ""
		return (successOrNothing output)

gitrevparse :: IO (Maybe Hash)
gitrevparse = do -- IO
		mresult <- safeRun "git" ["rev-parse", "--short", "HEAD"]
		let rev = do -- Maybe
			result <- mresult
			return (MkHash (init result))
		return rev

{- main -}

main :: IO ()
main = do -- IO
	status <- getContents
	mhash <- unsafeInterleaveIO gitrevparse -- defer the execution until we know we need the hash
	let result = do -- Maybe
		strings <- stringsFromStatus mhash status
		return (unwords strings)
	putStr (fromMaybe "" result)
