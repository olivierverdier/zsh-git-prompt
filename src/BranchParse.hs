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
	fmap (\ s -> ((Just s, Nothing), Nothing))
		$ string "Initial commit on " *> many anyChar <* eof

noBranch :: Parser BranchInfo
noBranch = 
	noBranchInfo
		<$ many (noneOf "(") <* string "(no branch)" <* eof

trackedBranch :: Parser Branch
trackedBranch = manyTill anyChar (string "...")

branchRemoteTracking :: Parser BranchInfo
branchRemoteTracking = 
	(\ bn tracking behead -> ((Just bn, Just tracking), Just behead))
		<$> trackedBranch
		<*> many (noneOf " ") <* char ' '
		<*> inBrackets

branchRemote :: Parser BranchInfo
branchRemote = 
	(\ bn tracking -> ((Just bn, Just tracking), Nothing))
		<$> trackedBranch
		<*> many (noneOf " ") <* eof

branchOnly :: Parser BranchInfo
branchOnly = 
	(\ bn -> ((Just bn, Nothing), Nothing))
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
makeAheadBehind name cons = 
	cons . read <$> (string (name ++ " ") *> many1 digit)

ahead :: Parser AheadBehind
ahead = makeAheadBehind "ahead" (\ n -> (n,0))
behind :: Parser AheadBehind
behind = makeAheadBehind "behind" (\ n -> (0,n))
aheadBehind :: Parser AheadBehind
aheadBehind =
	(\ (a,_) (_,b) -> (a,b))
		<$> ahead
		<* string ", "
		<*> behind

branchInfo :: String -> Either ParseError BranchInfo
branchInfo = parse branchParser ""
