import BranchParse (BranchInfo, branchInfo, AheadBehind)
import Text.Parsec (ParseError)
import Test.QuickCheck (Arbitrary(arbitrary), property, stdArgs, maxSuccess, quickCheckWith, suchThat, oneof, getPositive)
import Data.List (isPrefixOf, isSuffixOf, isInfixOf)
import Control.Applicative ((<$>), (<*>), pure)

{- ValidBranch type -}

newtype ValidBranch = MkBranch String deriving (Show, Eq)

isValidBranch :: String -> Bool
isValidBranch b = not . or $ [null, (' ' `elem`), (".." `isInfixOf`), ("." `isPrefixOf`), ("." `isSuffixOf`)] <*> pure b

instance Arbitrary ValidBranch where
	arbitrary = MkBranch <$> arbitrary `suchThat` isValidBranch

{- ahead/behind information -}

data BeHead = Ahead Int | Behind Int | AheadBehind Int Int deriving (Eq)

instance Show BeHead where
	show (Ahead i) = "[ahead " ++ (show i) ++ "]"
	show (Behind i) = "[behind " ++ (show i) ++ "]"
	show (AheadBehind i j) ="[ahead " ++ (show i) ++ ", behind " ++ (show j) ++ "]"

instance Arbitrary BeHead where
	arbitrary = oneof [Ahead <$> pos, Behind <$> pos, AheadBehind <$> pos <*> pos]
		where
			pos = getPositive <$> arbitrary

expectedAheadBehind :: BeHead -> AheadBehind
expectedAheadBehind behead = case behead of
	Ahead i -> (i,0)
	Behind j -> (0,j)
	AheadBehind i j -> (i,j)

{- Helper to tackle the Either type -}

expectRight :: BranchInfo -> Either ParseError BranchInfo -> Bool
expectRight expected computed = case computed of
	Left _ -> False
	Right res -> res == expected

{- Test -}

propNoBranch :: ValidBranch -> Bool
-- propNoBranch s = '(' `notElem` s ==> expectRight ((Nothing, Nothing), Nothing) $ branchInfo $ s ++ " (no branch)"
propNoBranch (MkBranch s) = expectRight ((Nothing, Nothing), Nothing) $ branchInfo $ s ++ " (no branch)"

propNewRepo :: ValidBranch -> Bool
-- propNewRepo s = notNull s ==> expectRight ((Just s, Nothing), Nothing) $ branchInfo $  "Initial commit on " ++ s
propNewRepo (MkBranch s) = expectRight ((Just s, Nothing), Nothing) $ branchInfo $  "Initial commit on " ++ s

propBranchOnly :: ValidBranch -> Bool
-- propBranchOnly s = ' ' `notElem` s ==> expectRight ((Just s, Nothing), Nothing) $ branchInfo s
propBranchOnly (MkBranch s) = expectRight ((Just s, Nothing), Nothing) $ branchInfo s

propBranchRemote :: ValidBranch -> ValidBranch -> Bool
-- propBranchRemote b t = (isValidBranch b && isValidBranch t && (not $ "..." `isInfixOf` b))  ==> expectRight ((Just b, Just t), Nothing) $ branchInfo $ b ++"..." ++ t 
propBranchRemote (MkBranch b) (MkBranch t) = expectRight ((Just b, Just t), Nothing) $ branchInfo $ b ++"..." ++ t 

propBranchRemoteTracking :: ValidBranch -> ValidBranch -> BeHead -> Bool
propBranchRemoteTracking (MkBranch b) (MkBranch t) behead = 
		expectRight ((Just b, Just t), Just $ expectedAheadBehind behead) result
	where
		result = branchInfo $ b ++ "..." ++ t ++ " " ++ (show behead)

main :: IO()
main = mapM_ (quickCheckWith stdArgs { maxSuccess = 2^8 }) [property propNoBranch,
				  property propNewRepo,
				  property propBranchOnly,
				  property propBranchRemote,
				  property propBranchRemoteTracking]

