module BranchParse where

import Control.Applicative ((<$>), (<*>), (<*), (*>), (<$), pure)
import Text.Parsec (digit, string, char, eof, anyChar, 
				   many, many1, manyTill, noneOf, between,
				   parse, ParseError, (<|>), try)
import Text.Parsec.String (Parser)
import Test.QuickCheck (Arbitrary(arbitrary), oneof, getPositive, suchThat)
import Data.List (isPrefixOf, isSuffixOf, isInfixOf)

{-
 The idea is to parse the first line of the git status command.
 Such a line may look like:
 	## master
or
 	## master...origin/master
or
 	## master...origin/master [ahead 3, behind 4]
 -}

data Distance = Ahead Int | Behind Int | AheadBehind Int Int deriving (Eq)

instance Show Distance where
	show (Ahead i) = "[ahead " ++ show i ++ "]"
	show (Behind i) = "[behind " ++ show i ++ "]"
	show (AheadBehind i j) ="[ahead " ++ show i ++ ", behind " ++ show j ++ "]"

instance Arbitrary Distance where
	arbitrary = oneof [
				   Ahead <$> pos,
				   Behind <$> pos,
				   AheadBehind <$> pos <*> pos]
		where
			pos = getPositive <$> arbitrary

{- Branch type -}

newtype Branch = MkBranch String deriving (Eq)

instance Show Branch where
		show (MkBranch b) = b

isValidBranch :: String -> Bool
isValidBranch b = not . or $ [null,
							 (' ' `elem`),
							 (".." `isInfixOf`),
							 ("." `isPrefixOf`),
							 ("." `isSuffixOf`)]
							 <*> pure b

instance Arbitrary Branch where
	arbitrary = MkBranch <$> arbitrary `suchThat` isValidBranch

data Remote = MkRemote Branch (Maybe Distance) deriving (Eq, Show)

getDistance :: Remote -> Maybe Distance
getDistance (MkRemote _ md) = md

data BranchInfo = MkBranchInfo Branch (Maybe Remote) deriving (Eq, Show)

type MBranchInfo = Maybe BranchInfo

newRepo :: Parser MBranchInfo
newRepo = 
	fmap (\ branch -> Just $ MkBranchInfo (MkBranch branch) Nothing)
		$ string "Initial commit on " *> many anyChar <* eof

noBranch :: Parser MBranchInfo
noBranch = 
	Nothing
		<$ manyTill anyChar (try $ string " (no branch)") <* eof

trackedBranch :: Parser Branch
trackedBranch = MkBranch <$> manyTill anyChar (try $ string "...")

branchRemoteTracking :: Parser MBranchInfo
branchRemoteTracking = 
	(\ branch tracking behead -> Just $ MkBranchInfo branch $ Just $ MkRemote (MkBranch tracking) (Just behead))
		<$> trackedBranch
		<*> many (noneOf " ") <* char ' '
		<*> inBrackets

branchRemote :: Parser MBranchInfo
branchRemote = 
	(\ branch tracking -> Just $ MkBranchInfo branch $ Just $ MkRemote (MkBranch tracking) Nothing)
		<$> trackedBranch
		<*> many (noneOf " ") <* eof

branchOnly :: Parser MBranchInfo
branchOnly = 
	(\ branch -> Just $ MkBranchInfo (MkBranch branch) Nothing)
		<$> many (noneOf " ") <* eof

branchParser :: Parser MBranchInfo
branchParser = 
			try noBranch
		<|> try newRepo
		<|> try branchRemoteTracking
		<|> try branchRemote
		<|> branchOnly

branchParser' :: Parser MBranchInfo
branchParser' = (string "## ") >> branchParser

inBrackets :: Parser Distance
inBrackets = between (char '[') (char ']') (behind <|> try aheadBehind <|> ahead)

makeAheadBehind :: String -> (Int -> Distance) -> Parser Distance
makeAheadBehind name constructor = 
	constructor . read <$> (string (name ++ " ") *> many1 digit)

ahead :: Parser Distance
ahead = makeAheadBehind "ahead" Ahead
behind :: Parser Distance
behind = makeAheadBehind "behind" Behind
aheadBehind :: Parser Distance
aheadBehind =
	(\ (Ahead aheadBy) (Behind behindBy) -> AheadBehind aheadBy behindBy)
		<$> ahead
		<* string ", "
		<*> behind

branchInfo :: String -> Either ParseError MBranchInfo
branchInfo = parse branchParser' ""

pairFromDistance :: Distance -> (Int, Int)
pairFromDistance (Ahead n) = (n,0)
pairFromDistance (Behind n) = (0,n)
pairFromDistance (AheadBehind m n) = (m,n)
