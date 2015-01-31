import BranchParse (BranchInfo(MkBranchInfo), branchInfo, Distance, Branch(MkBranch), noBranchInfo)
import Test.QuickCheck (property, stdArgs, maxSuccess, quickCheckWith)


{- Helper to tackle the Either type -}

checkRight :: BranchInfo -> String -> Bool
checkRight b s = expectRight b $ branchInfo s
	where
		expectRight expected computed = case computed of
			Left _ -> False
			Right res -> res == expected

{- Test -}

propNoBranch :: Branch -> Bool
propNoBranch (MkBranch s) =
		checkRight
			noBranchInfo
			$ s ++ " (no branch)"

propNewRepo :: Branch -> Bool
propNewRepo b@(MkBranch s) =
		checkRight 
			(MkBranchInfo (Just b) Nothing Nothing)
			$ "Initial commit on " ++ s

propBranchOnly :: Branch -> Bool
propBranchOnly b@(MkBranch s) = 
		checkRight 
			(MkBranchInfo (Just b) Nothing Nothing)
			s

propBranchRemote :: Branch -> Branch -> Bool
propBranchRemote b'@(MkBranch b) t'@(MkBranch t) =
		checkRight
			(MkBranchInfo (Just b') (Just t') Nothing)
			$ b ++"..." ++ t 

propBranchRemoteTracking :: Branch -> Branch -> Distance -> Bool
propBranchRemoteTracking b'@(MkBranch b) t'@(MkBranch t) distance = 
		checkRight 
			(MkBranchInfo (Just b') (Just t') (Just distance))
		 	$ b ++ "..." ++ t ++ " " ++ show distance

main :: IO()
main = mapM_ (quickCheckWith stdArgs { maxSuccess = 2^8 }) [
				property propNoBranch,
				property propNewRepo,
				property propBranchOnly,
				property propBranchRemote,
				property propBranchRemoteTracking]

