module BranchParse where

import Control.Applicative (liftA, liftA2)
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
				   liftA Ahead  pos,
				   liftA Behind pos,
				   liftA2 AheadBehind pos pos]
		where
			pos = liftA getPositive arbitrary

{- Branch type -}

newtype Branch = MkBranch String deriving (Eq)

instance Show Branch where
		show (MkBranch b) = b

isValidBranch :: String -> Bool
isValidBranch b = not (or (isForbidden b)) where
	isForbidden s = do -- List
		forbidden <- [null, (' ' `elem`), (".." `isInfixOf`), ("." `isPrefixOf`), ("." `isSuffixOf`)]
		return (forbidden s)


instance Arbitrary Branch where
	arbitrary =
		do -- Gen
			branch <- arbitrary `suchThat` isValidBranch
			return (MkBranch branch)

data Remote = MkRemote Branch (Maybe Distance) deriving (Eq, Show)

getDistance :: Remote -> Maybe Distance
getDistance (MkRemote _ md) = md

data BranchInfo = MkBranchInfo Branch (Maybe Remote) deriving (Eq, Show)

type MBranchInfo = Maybe BranchInfo

newRepo :: Parser MBranchInfo
newRepo =
	do -- Parsec
		string "Initial commit on "
		branchOnly

noBranch :: Parser MBranchInfo
noBranch =
	do -- Parsec
		manyTill anyChar (try (string " (no branch)"))
		eof
		return Nothing

trackedBranch :: Parser Branch
trackedBranch =
		do -- Parsec
			b <- manyTill anyChar (try (string "..."))
			return (MkBranch b)

branchRemoteTracking :: Parser MBranchInfo
branchRemoteTracking =
	do -- Parsec
		branch <- trackedBranch
		tracking <- many (noneOf " ")
		char ' '
		behead <- inBrackets
		let remote = MkRemote (MkBranch tracking) (Just behead)
		let bi = MkBranchInfo branch  (Just remote)
		return (Just bi)


branchRemote :: Parser MBranchInfo
branchRemote =
	do -- Parsec
		branch <- trackedBranch
		tracking <- many (noneOf " ")
		eof
		let remote = MkRemote (MkBranch tracking) Nothing
		let bi = MkBranchInfo branch (Just remote)
		return (Just bi)

branchOnly :: Parser MBranchInfo
branchOnly =
	do -- Parsec
		branch <- many (noneOf " ")
		eof
		let bi = MkBranchInfo (MkBranch branch) Nothing
		return (Just bi)

branchParser :: Parser MBranchInfo
branchParser =
			try noBranch
		<|> try newRepo
		<|> try branchRemoteTracking
		<|> try branchRemote
		<|> branchOnly

branchParser' :: Parser MBranchInfo
branchParser' =
	do -- Parsec
		string "## "
		branchParser

inBrackets :: Parser Distance
inBrackets = between (char '[') (char ']') (behind <|> try aheadBehind <|> ahead)

makeAheadBehind :: String -> (Int -> Distance) -> Parser Distance
makeAheadBehind name constructor =
	do -- Parsec
		string (name ++ " ")
		dist <- many1 digit
		return (constructor (read dist))

ahead :: Parser Distance
ahead = makeAheadBehind "ahead" Ahead
behind :: Parser Distance
behind = makeAheadBehind "behind" Behind
aheadBehind :: Parser Distance
aheadBehind =
	do -- Parsec
		Ahead aheadBy <- ahead
		string ", "
		Behind behindBy <- behind
		return (AheadBehind aheadBy behindBy)

branchInfo :: String -> Either ParseError MBranchInfo
branchInfo = parse branchParser' ""

pairFromDistance :: Distance -> (Int, Int)
pairFromDistance (Ahead n) = (n,0)
pairFromDistance (Behind n) = (0,n)
pairFromDistance (AheadBehind m n) = (m,n)
