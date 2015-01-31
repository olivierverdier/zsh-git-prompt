import BranchParse (BranchInfo, branchInfo, Distance)
import Test.QuickCheck (Arbitrary(arbitrary), property, stdArgs, maxSuccess, quickCheckWith, suchThat)
import Data.List (isPrefixOf, isSuffixOf, isInfixOf)
import Control.Applicative ((<$>), (<*>), pure)

{- ValidBranch type -}

newtype ValidBranch = MkBranch String deriving (Show, Eq)

isValidBranch :: String -> Bool
isValidBranch b = not . or $ [null,
							 (' ' `elem`),
							 (".." `isInfixOf`),
							 ("." `isPrefixOf`),
							 ("." `isSuffixOf`)]
							 <*> pure b

instance Arbitrary ValidBranch where
	arbitrary = MkBranch <$> arbitrary `suchThat` isValidBranch


{- Helper to tackle the Either type -}

checkRight :: BranchInfo -> String -> Bool
checkRight b s = expectRight b $ branchInfo s
	where
		expectRight expected computed = case computed of
			Left _ -> False
			Right res -> res == expected

{- Test -}

propNoBranch :: ValidBranch -> Bool
propNoBranch (MkBranch s) =
		checkRight
			((Nothing, Nothing), Nothing)
			$ s ++ " (no branch)"

propNewRepo :: ValidBranch -> Bool
propNewRepo (MkBranch s) =
		checkRight 
			((Just s, Nothing), Nothing)
			$ "Initial commit on " ++ s

propBranchOnly :: ValidBranch -> Bool
propBranchOnly (MkBranch s) = 
		checkRight 
			((Just s, Nothing), Nothing)
			s

propBranchRemote :: ValidBranch -> ValidBranch -> Bool
propBranchRemote (MkBranch b) (MkBranch t) =
		checkRight
			((Just b, Just t), Nothing)
			$ b ++"..." ++ t 

propBranchRemoteTracking :: ValidBranch -> ValidBranch -> Distance -> Bool
propBranchRemoteTracking (MkBranch b) (MkBranch t) distance = 
		checkRight 
			((Just b, Just t), Just distance)
		 	$ b ++ "..." ++ t ++ " " ++ show distance

main :: IO()
main = mapM_ (quickCheckWith stdArgs { maxSuccess = 2^8 }) [
				property propNoBranch,
				property propNewRepo,
				property propBranchOnly,
				property propBranchRemote,
				property propBranchRemoteTracking]

