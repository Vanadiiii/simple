module ListsHelper (module ListsHelper) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- 1) Find the last element of a list
myLast :: [a] -> a
myLast [] = error "empty list"
myLast [x] = x
myLast (_ : xs) = myLast xs

-- 2) Find the last but one element of a list.
myButLast :: [a] -> a
myButLast [] = error "list is empty"
myButLast [_] = error "list's size is less then 2"
myButLast (x : [_]) = x
myButLast (_ : xs) = myButLast xs

-- 3) Find the K'th element of a list. The first element in the list is number 1.
elementAt :: [a] -> Int -> a
elementAt [] _ = error "list is empty"
elementAt (x : _) 1 = x
elementAt (_ : xs) k = elementAt xs (k - 1)

-- 4) Find the number of elements of a list.
myLength :: [a] -> Integer
myLength [] = 0
myLength (_ : xs) = 1 + myLength xs

-- 5) Reverse a list
myReverse :: [a] -> [a]
myReverse = myReverse' []
  where
    myReverse' acc [] = acc
    myReverse' acc (x : xs) = myReverse' (x : acc) xs

-- 6) Find out whether a list is a palindrome. A palindrome can be read forward or backward; e.g. (x a m a x).
isPalindrome :: Eq a => [a] -> Bool
isPalindrome [] = True
isPalindrome word = and $ zipWith (==) word (reverse word)

-- 7) Flatten a nested list structure
--    Transform a list, possibly holding lists as elements into a `flat' list by replacing each list with its elements (recursively)
data NestedList a = Elem a | List [NestedList a] deriving (Show)

flatten :: NestedList a -> [a]
flatten (Elem a) = [a]
flatten (List []) = []
flatten (List xs) = concatMap flatten xs

-- 8) Eliminate consecutive duplicates of list elements.
--    If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed.
compress :: Eq a => [a] -> [a]
compress [] = []
compress [a] = [a]
compress (x : y : xs) =
  if x == y
    then compress (y : xs)
    else x : compress (y : xs)

-- 9) Pack consecutive duplicates of list elements into sublists. If a list contains repeated elements they should be placed in separate sublists.
pack :: Eq a => [a] -> [[a]]
pack = pack' [] []
  where
    pack' acc1 [] [] = acc1
    pack' acc1 acc2 [] = acc1 ++ [acc2]
    pack' acc1 [] (x : xs) = pack' acc1 [x] xs
    pack' acc1 acc2@(a : _) (x : xs) =
      if x == a
        then pack' acc1 (x : acc2) xs
        else pack' (acc1 ++ [acc2]) [x] xs

-- 10) Run-length encoding of a list. Use the result of problem P09 to implement the so-called run-length encoding data compression method.
--     Consecutive duplicates of elements are encoded as lists (N E) where N is the number of duplicates of the element E.
encode :: Eq a => [a] -> [(Int, a)]
encode = map (\ls@(l : _) -> (length ls, l)) . pack

-- 11) Modify the result of problem 10 in such a way that if an element has no duplicates it is simply copied into the result list.
--     Only elements with duplicates are transferred as (N E) lists.
data ListItem a = Single a | Multiply Int a deriving (Eq, Show)

encodeModified :: Eq a => [a] -> [ListItem a]
encodeModified = map encodeHelper . encode
  where
    encodeHelper (1, x) = Single x
    encodeHelper (n, x) = Multiply n x

-- 12) Decode a run-length encoded list.
--     Given a run-length code list generated as specified in problem 11. Construct its uncompressed version.
decodeModified :: [ListItem a] -> [a]
decodeModified = concatMap decodeHelper
  where
    decodeHelper (Single a) = [a]
    decodeHelper (Multiply n a) = replicate n a
