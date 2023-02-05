import qualified Lib
import Test.HUnit

main :: IO ()
main = do
  runTestTTAndExit myLastTests

myLastTests :: Test
myLastTests =
  TestList
    [ TestLabel "last of many elements in list" myLastTest1,
      TestLabel "only one from list" myLastTest2
--      TestLabel "empty list" myLastTest3 --todo - fix
    ]

myLastTest1 :: Test
myLastTest1 = TestCase (assertEqual "last of full list is 42" 42 (Lib.myLast [1, 2, 3, 42])) --todo - fix warning

myLastTest2 :: Test
myLastTest2 = TestCase (assertEqual "last of list of single element is 'a'" 'a' (Lib.myLast ['a']))

--myLastTest3 :: Test
--myLastTest3 = TestCase (assertEqual "last of empty list is failure" 1 (Lib.myLast []))
