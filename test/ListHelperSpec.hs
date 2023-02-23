import ListsHelper
import Test.HUnit

main :: IO ()
main = do
  runTestTTAndExit $
    TestList
      [ myLastTests,
        myButLastTests,
        elementAtTests,
        myLengthTests,
        myReverseTests,
        flattenTests,
        compressTests,
        packTests,
        encodeTests,
        encodeModifiedTests,
        decodeModifiedTests,
        encodeDirectTests,
        dupliTests,
        repliTests,
        dropEveryTests,
        isPalindromeTests
      ]

myLastTests :: Test
myLastTests =
  TestList
    [ TestLabel "last of many elements in list" myLastTest1,
      TestLabel "only one from list" myLastTest2
    ]
  where
    myLastTest1 = TestCase . assertEqual "last of full list is 42" 42 $ ListsHelper.myLast [1, 2, 3, 42]
    myLastTest2 = TestCase . assertEqual "last of list of single element is 'a'" 'a' $ ListsHelper.myLast ['a']

myButLastTests :: Test
myButLastTests =
  TestList
    [ TestLabel "last but one of list with size more than 2" myButLastTest1,
      TestLabel "last but one of list with 2 elements" myButLastTest2
    ]
  where
    myButLastTest1 = TestCase . assertEqual "last but one is 42" 42 $ ListsHelper.myButLast [1, 2, 42, 43]
    myButLastTest2 = TestCase . assertEqual "last but one is 42" 42 $ ListsHelper.myButLast [42, 43]

elementAtTests :: Test
elementAtTests =
  TestList
    [ TestLabel "find first in single list" elementAtTest1,
      TestLabel "find third in list" elementAtTest2
    ]
  where
    elementAtTest1 = TestCase . assertEqual "element at 1 is 42" 42 $ ListsHelper.elementAt [42] 1
    elementAtTest2 = TestCase . assertEqual "element at 3 is 42" 42 $ ListsHelper.elementAt [0, 1, 42] 3

myLengthTests :: Test
myLengthTests =
  TestList
    [ TestLabel "empty list length" myLengthTest1,
      TestLabel "not empty list length" myLengthTest2
    ]
  where
    myLengthTest1 = TestCase . assertEqual "empty list's length is 0" 0 $ ListsHelper.myLength []
    myLengthTest2 = TestCase . assertEqual "list's size is 5" 5 $ ListsHelper.myLength [42, 42, 42, 42, 42]

myReverseTests :: Test
myReverseTests =
  TestList
    [ TestLabel "reverse empty list" myReverseTest1,
      TestLabel "reverse not empty list" myReverseTest2
    ]
  where
    myReverseTest1 = TestCase . assertEqual "reversed empty string is empty string" "" $ ListsHelper.myReverse ""
    myReverseTest2 = TestCase . assertEqual "reversed string id '!olleH'" "!olleH" $ ListsHelper.myReverse "Hello!"

isPalindromeTests :: Test
isPalindromeTests =
  TestList
    [ TestLabel "empty string is palindrome" myReverseTest1,
      TestLabel "palindrome" myReverseTest2,
      TestLabel "not palindrome" myReverseTest3
    ]
  where
    myReverseTest1 = TestCase . assertBool "empty string is palindrome" $ ListsHelper.isPalindrome ""
    myReverseTest2 = TestCase . assertBool "'abba' is palindrome" $ ListsHelper.isPalindrome "abba"
    myReverseTest3 = TestCase . assertBool "'Hello!' isn't palindrome" . not $ ListsHelper.isPalindrome "Hello!"

flattenTests :: Test
flattenTests =
  TestList
    [ TestLabel "single elem" flattenTest1,
      TestLabel "empty node" flattenTest2,
      TestLabel "complex node" flattenTest3
    ]
  where
    flattenTest1 = TestCase . assertEqual "flatten of (12) is 12" [12] $ ListsHelper.flatten $ Elem 12
    flattenTest2 = TestCase . assertBool "flatten of () is empty" $ null $ ListsHelper.flatten (List [])
    list = List [Elem 1, List [Elem 12, List [Elem 33, Elem 42]]]
    flattenTest3 = TestCase . assertEqual "flatten of (1, (12, (33, 42)) is [1, 12, 33, 42]" [1, 12, 33, 42] $ ListsHelper.flatten list

compressTests :: Test
compressTests =
  TestList
    [ TestLabel "empty list" compressTest1,
      TestLabel "list with single elem" compressTest2,
      TestLabel "full list" compressTest3
    ]
  where
    compressTest1 = TestCase . assertBool "compress of '' is ''" . null $ ListsHelper.compress ""
    compressTest2 = TestCase . assertEqual "compress of [42] is [42]" [42] $ ListsHelper.compress [42]
    compressTest3 = TestCase . assertEqual "compress of [42, 42, 13, 13, 13, 1, 1, 1, 1] is [42, 13, 1]" [42, 13, 1] $ ListsHelper.compress [42, 42, 13, 13, 13, 1, 1, 1, 1]

packTests :: Test
packTests =
  TestList
    [ TestLabel "empty list" packTest1,
      TestLabel "list with single elem" packTest2,
      TestLabel "list with many elems" packTest3
    ]
  where
    packTest1 = TestCase . assertBool "pack of [] is []" . null $ ListsHelper.pack ([] :: [Int])
    packTest2 = TestCase . assertEqual "pack of [42] is [[42]]" [[42]] $ ListsHelper.pack [42]
    packTest3 = TestCase . assertEqual "pack of [1,2,3,3,3,4,4,5] is [[1],[2],[3,3,3],[4,4],[5]]" [[1], [2], [3, 3, 3], [4, 4], [5]] $ ListsHelper.pack [1, 2, 3, 3, 3, 4, 4, 5]

encodeTests :: Test
encodeTests =
  TestList
    [ TestLabel "empty list" encodeTest1,
      TestLabel "list with single elem" encodeTest2,
      TestLabel "list with many elems" encodeTest3
    ]
  where
    encodeTest1 = TestCase . assertBool "encode of [] is []" . null $ ListsHelper.encode ([] :: [Int])
    encodeTest2 = TestCase . assertEqual "encode of [42] is [[42]]" [(1, 42)] $ ListsHelper.encode [42]
    encodeTest3 = TestCase . assertEqual "encode of [1,2,3,3,3,4,4,5] is [(1,1),(1,2),(3,3),(2,4),(1,5)]" [(1, 1), (1, 2), (3, 3), (2, 4), (1, 5)] $ ListsHelper.encode [1, 2, 3, 3, 3, 4, 4, 5]

encodeModifiedTests :: Test
encodeModifiedTests =
  TestList
    [ TestLabel "empty list" encodeModifiedTest1,
      TestLabel "list with single elem" encodeModifiedTest2,
      TestLabel "list with many elems" encodeModifiedTest3
    ]
  where
    encodeModifiedTest1 = TestCase . assertBool "encodeModified of [] is []" . null $ ListsHelper.encodeModified ([] :: [Int])
    encodeModifiedTest2 = TestCase . assertEqual "encodeModified of [42] is [Single 42]" [Single 42] $ ListsHelper.encodeModified ([42] :: [Int])
    encodeModifiedTest3 = TestCase . assertEqual "encodeModified of [3,3,4,5] is [Multiply 2 3, Single 4, Single 5]" [Multiply 2 3, Single 4, Single 5] $ ListsHelper.encodeModified ([3, 3, 4, 5] :: [Int])

decodeModifiedTests :: Test
decodeModifiedTests =
  TestList
    [ TestLabel "empty list" decodeModifiedTest1,
      TestLabel "list with single elem" decodeModifiedTest2,
      TestLabel "list with many elems" decodeModifiedTest3
    ]
  where
    decodeModifiedTest1 = TestCase . assertBool "decodeModified of [] is []" . null $ ListsHelper.decodeModified ([] :: [ListItem Int])
    decodeModifiedTest2 = TestCase . assertEqual "decodeModified of [Single 42] is [42]" [42] $ ListsHelper.decodeModified [Single 42]
    decodeModifiedTest3 = TestCase . assertEqual "decodeModified of [Multiply 2 3, Single 4, Single 5] is [3,3,4,5]" [3, 3, 4, 5] $ ListsHelper.decodeModified [Multiply 2 3, Single 4, Single 5]

encodeDirectTests :: Test
encodeDirectTests =
  TestList
    [ TestLabel "empty list" encodeDirectTest1,
      TestLabel "list with single elem" encodeDirectTest2,
      TestLabel "list with many elems" encodeDirectTest3
    ]
  where
    encodeDirectTest1 = TestCase . assertBool "encodeDirect of [] is []" . null $ ListsHelper.encodeDirect ([] :: [Int])
    encodeDirectTest2 = TestCase . assertEqual "encodeDirect of [42] is [Single 42]" [Single 42] $ ListsHelper.encodeDirect ([42] :: [Int])
    encodeDirectTest3 = TestCase . assertEqual "encodeDirect of [3,3,4,5] is [Multiply 2 3, Single 4, Single 5]" [Multiply 2 3, Single 4, Single 5] $ ListsHelper.encodeDirect ([3, 3, 4, 5] :: [Int])

dupliTests :: Test
dupliTests =
  TestList
    [ TestLabel "empty list duplication" dupliTest1,
      TestLabel "not empty list duplication" dupliTest2
    ]
  where
    dupliTest1 = TestCase . assertEqual "dupli [] is []" [] $ ListsHelper.dupli ([] :: [Int])
    dupliTest2 = TestCase . assertEqual "dupli [1, 2, 3] is [1,1,2,2,3,3]" [1, 1, 2, 2, 3, 3] $ ListsHelper.dupli [1, 2, 3]

repliTests :: Test
repliTests =
  TestList
    [ TestLabel "empty list replication" repliTest1,
      TestLabel "not empty list replication 1" repliTest2,
      TestLabel "not empty list replication 2" repliTest3
    ]
  where
    repliTest1 = TestCase . assertEqual "repli [] _ is []" [] $ ListsHelper.repli ([] :: [Int]) 42
    repliTest2 = TestCase . assertEqual "repli [1,2,3] 2 is [1,1,2,2,3,3]" [1, 1, 2, 2, 3, 3] $ ListsHelper.repli [1, 2, 3] 2
    repliTest3 = TestCase . assertEqual "repli [1, 2, 3] 1 is [1,2,3]" [1, 2, 3] $ ListsHelper.repli [1, 2, 3] 1

dropEveryTests :: Test
dropEveryTests =
  TestList
    [ TestLabel "empty list dropping" dropEveryTest1,
      TestLabel "not empty list dropping 1" dropEveryTest2
    ]
  where
    dropEveryTest1 = TestCase . assertEqual "dropEvery [] _ is []" [] $ ListsHelper.dropEvery ([] :: [Int]) 42
    dropEveryTest2 = TestCase . assertEqual "dropEvery [1,2,3] 2 is [1,3]" [1, 3] $ ListsHelper.dropEvery [1, 2, 3] 2
