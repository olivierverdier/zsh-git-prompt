module BranchParse where

import Control.Applicative ((<$>), (<*>), (<*), (*>), (<$))
import Text.Parsec (digit, string, char, eof, anyChar, 
				   many, many1, manyTill, noneOf, between,
				   parse, ParseError, (<|>), try)
import Text.Parsec.String (Parser)
import Test.QuickCheck (Arbitrary(arbitrary), oneof, getPositive)

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

type Branch = String
type BranchInfo = ((Maybe Branch, Maybe Branch), Maybe Distance)

noBranchInfo :: BranchInfo
noBranchInfo = ((Nothing, Nothing), Nothing)

newRepo :: Parser BranchInfo
newRepo = 
	fmap (\ branch -> ((Just branch, Nothing), Nothing))
		$ string "Initial commit on " *> many anyChar <* eof

noBranch :: Parser BranchInfo
noBranch = 
	noBranchInfo
		<$ manyTill anyChar (try $ string " (no branch)") <* eof

trackedBranch :: Parser Branch
trackedBranch = manyTill anyChar (try $ string "...")

branchRemoteTracking :: Parser BranchInfo
branchRemoteTracking = 
	(\ branch tracking behead -> ((Just branch, Just tracking), Just behead))
		<$> trackedBranch
		<*> many (noneOf " ") <* char ' '
		<*> inBrackets

branchRemote :: Parser BranchInfo
branchRemote = 
	(\ branch tracking -> ((Just branch, Just tracking), Nothing))
		<$> trackedBranch
		<*> many (noneOf " ") <* eof

branchOnly :: Parser BranchInfo
branchOnly = 
	(\ branch -> ((Just branch, Nothing), Nothing))
		<$> many (noneOf " ") <* eof

branchParser :: Parser BranchInfo
branchParser = 
			try noBranch
		<|> try newRepo
		<|> try branchRemoteTracking
		<|> try branchRemote
		<|> branchOnly


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

branchInfo :: String -> Either ParseError BranchInfo
branchInfo = parse branchParser ""

pairFromDistance :: Distance -> (Int, Int)
pairFromDistance (Ahead n) = (n,0)
pairFromDistance (Behind n) = (0,n)
pairFromDistance (AheadBehind m n) = (m,n)
