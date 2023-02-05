import qualified Lib
import Test.HUnit

main :: IO ()
main = do
  runTestTTAndExit $
    TestList
      [ lastTests,
        lastButOneTests
      ]

-- todo - add error's checking!
lastTests :: Test
lastTests =
  TestList
    [ TestLabel "last of many elements in list" lastTest1,
      TestLabel "only one from list" lastTest2
    ]
  where
    lastTest1 = TestCase (assertEqual "last of full list is 42" 42 (Lib.last [1, 2, 3, 42])) --todo - fix warning
    lastTest2 = TestCase (assertEqual "last of list of single element is 'a'" 'a' (Lib.last ['a']))

-- todo - add error's checking!
lastButOneTests :: Test
lastButOneTests =
  TestList
    [ TestLabel "last but one of list with size more than 2" lastButOneTest1,
      TestLabel "last but one of list with 2 elements" lastButOneTest2
    ]
  where
    lastButOneTest1 = TestCase (assertEqual "last but one is 42" 42 (Lib.lastButOne [1, 2, 42, 43])) --todo - fix warning
    lastButOneTest2 = TestCase (assertEqual "last but one is 42" 42 (Lib.lastButOne [42, 43])) --todo - fix warning
