import BranchParse (BranchInfo(MkBranchInfo), branchInfo, Distance, Branch, noBranchInfo)
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
propNoBranch b =
		checkRight
			noBranchInfo
			$ show b ++ " (no branch)"

propNewRepo :: Branch -> Bool
propNewRepo b =
		checkRight 
			(MkBranchInfo (Just b) Nothing Nothing)
			$ "Initial commit on " ++ show b

propBranchOnly :: Branch -> Bool
propBranchOnly b = 
		checkRight 
			(MkBranchInfo (Just b) Nothing Nothing)
			$ show b

propBranchRemote :: Branch -> Branch -> Bool
propBranchRemote b t =
		checkRight
			(MkBranchInfo (Just b) (Just t) Nothing)
			$ show b ++"..." ++ show t 

propBranchRemoteTracking :: Branch -> Branch -> Distance -> Bool
propBranchRemoteTracking b t distance = 
		checkRight 
			(MkBranchInfo (Just b) (Just t) (Just distance))
		 	$ show b ++ "..." ++ show t ++ " " ++ show distance

main :: IO()
main = mapM_ (quickCheckWith stdArgs { maxSuccess = 2^8 }) [
				property propNoBranch,
				property propNewRepo,
				property propBranchOnly,
				property propBranchRemote,
				property propBranchRemoteTracking]

