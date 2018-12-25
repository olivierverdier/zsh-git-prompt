import Test.HUnit (Test(TestList), runTestTT, (~=?), Counts(errors, failures))
import Utils (stringsFromStatus, Hash(MkHash))
import System.Exit (exitFailure)
import Control.Monad (when)

type TestData = (String, String, [Int])

tests :: [TestData]
tests = [
		("## master...ori/master [ahead 3]\n M", "master", [3,0,0,0,1,0])
		,
		("## stat\nM ", "stat", [0,0,1,0,0,0])
		,
		("## exp...o/exp [ahead 3, behind 2]\n", "exp", [3,2,0,0,0,0])
		,
		("## master\nU \nU \nM \nM \nM ", "master", [0,0,3,2,0,0])
		,
		("## HEAD (no branch)\n", ":hash", [0,0,0,0,0,0])
		,
		("## master\n M\n M\n M\n??\n", "master", [0,0,0,0,3,1])
		,
		("## dev...o/dev [ahead 4, behind 5]\nM \n M\n??\n", "dev", [4,5,1,0,1,1])
		,
		("## dev...origin/master [ahead 4, behind 5]\nMM foo\n?? bar\n", "dev", [4,5,1,0,1,1])
		]

makeTest :: TestData -> Test
makeTest (input, branch, numbers) = Just (branch : fmap show numbers) ~=? stringsFromStatus (Just (MkHash "hash")) input

main :: IO ()
main = do -- IO
	testResult <- (runTestTT . TestList . fmap makeTest) tests
	let some accessor = accessor testResult /= 0 in
		when (some errors  || some failures) exitFailure
