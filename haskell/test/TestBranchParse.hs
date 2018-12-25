import BranchParse (MBranchInfo, BranchInfo(MkBranchInfo), branchInfo, Distance, Branch, Remote(MkRemote))
import Test.QuickCheck (property, stdArgs, maxSuccess, quickCheckWithResult, Result, Property)
import Test.QuickCheck.Test (isSuccess)
import System.Exit (exitFailure)
import Control.Monad (forM, unless)


{- Helper to tackle the Either type -}

checkRight :: MBranchInfo -> String -> Bool
checkRight b s = expectRight b (branchInfo ("## " ++ s))
	where
		expectRight expected computed = case computed of
			Left _ -> False
			Right res -> res == expected

-- gitRep :: BranchInfo -> String
-- gitRep (MkBranchInfo (MkBranch branch) Nothing) = branch
-- gitRep (MkBranchInfo (MkBranch branch) (MkRemote (Just (MkBranch rem)) Nothing)) = branch ++ "..." ++ rem
-- gitRep (MkBranchInfo (MkBranch branch) (MkRemote (Just (MkBranch rem)) (Just dis))) = gitRep (MkBranchInfo branch $ MkRemote (Just rem) Nothing) ++ show dis
--
{- Test -}

propNoBranch :: Branch -> Bool
propNoBranch b =
		checkRight
			Nothing
			(show b ++ " (no branch)")

propNewRepo :: Branch -> Bool
propNewRepo b =
		checkRight
			(Just (MkBranchInfo b Nothing))
			("Initial commit on " ++ show b)

propBranchOnly :: Branch -> Bool
propBranchOnly b =
		checkRight
			(Just (MkBranchInfo b Nothing))
			(show b)

propBranchRemote :: Branch -> Branch -> Bool
propBranchRemote b t =
		checkRight
			(Just (MkBranchInfo b remote))
			(show b ++"..." ++ show t)
		where
			remote = Just (MkRemote t Nothing)

propBranchRemoteTracking :: Branch -> Branch -> Distance -> Bool
propBranchRemoteTracking b t distance =
		checkRight
			(Just (MkBranchInfo b remote))
		 	(show b ++ "..." ++ show t ++ " " ++ show distance)
		where
			remote = Just (MkRemote t (Just distance))


allTests :: [Property]
allTests = [
				property propNoBranch,
				property propNewRepo,
				property propBranchOnly,
				property propBranchRemote,
				property propBranchRemoteTracking
				]

runTests :: IO [Result]
runTests = forM allTests runTest
	where
		runTest = quickCheckWithResult stdArgs { maxSuccess = 256 }

main :: IO()
main = do -- IO
	results <- runTests
	unless (all isSuccess results) exitFailure
