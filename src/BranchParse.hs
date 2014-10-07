module BranchParse (branchInfo, BranchInfo, noBranchInfo, Branch) where

import Control.Applicative hiding ((<|>), many, optional)
import Text.Parsec
import Text.Parsec.String

{-
 The idea is to parse the first line of the git status command.
 Such a line may look like:
 	## master
or
 	## master...origin/master
or
 	## master...origin/master [ahead 3, behind 4]
 -}

type AheadBehind = (Int, Int)
type Branch = String
type BranchInfo = ((Maybe Branch, Maybe Branch), Maybe AheadBehind)

noBranchInfo :: BranchInfo
noBranchInfo = ((Nothing, Nothing), Nothing)

newRepo :: Parser BranchInfo
newRepo = 
	fmap (\ branch -> ((Just branch, Nothing), Nothing))
		$ string "Initial commit on " *> many anyChar <* eof

noBranch :: Parser BranchInfo
noBranch = 
	noBranchInfo
		<$ many (noneOf "(") <* string "(no branch)" <* eof

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


inBrackets :: Parser AheadBehind
inBrackets = between (char '[') (char ']') (behind <|> try aheadBehind <|> ahead)

makeAheadBehind :: String -> (Int -> AheadBehind) -> Parser AheadBehind
makeAheadBehind name constructor = 
	constructor . read <$> (string (name ++ " ") *> many1 digit)

ahead :: Parser AheadBehind
ahead = makeAheadBehind "ahead" (\ n -> (n,0))
behind :: Parser AheadBehind
behind = makeAheadBehind "behind" (\ n -> (0,n))
aheadBehind :: Parser AheadBehind
aheadBehind =
	(\ (aheadBy,_) (_,behindBy) -> (aheadBy, behindBy))
		<$> ahead
		<* string ", "
		<*> behind

branchInfo :: String -> Either ParseError BranchInfo
branchInfo = parse branchParser ""
