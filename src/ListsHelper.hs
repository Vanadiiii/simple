module ListsHelper (module ListsHelper) where

import Control.Monad (replicateM)
import System.Random

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

-- 13) Run-length encoding of a list (direct solution).
--     Implement the so-called run-length encoding data compression method directly.
--     I.e. don't explicitly create the sublists containing the duplicates, as in problem 9, but only count them.
--     As in problem P11, simplify the result list by replacing the singleton lists (1 X) by X.
encodeDirect :: Eq a => [a] -> [ListItem a]
encodeDirect [] = []
encodeDirect [x] = [Single x]
encodeDirect (x : xs) = encodeIn (x, 1) xs
  where
    encodeIn (a, count) [] =
      if count == 1
        then [Single a]
        else [Multiply count a]
    encodeIn (a, count) (b : bs) =
      if a == b
        then encodeIn (a, count + 1) bs
        else encodeIn (a, count) [] ++ encodeIn (b, 1) bs

-- 14) Duplicate the elements of a list
dupli :: [a] -> [a]
dupli [] = []
dupli (x : xs) = x : x : dupli xs

-- 15) Replicate the elements of a list a given number of times
repli :: [a] -> Int -> [a]
repli [] _ = []
repli (x : xs) n = repli' x n ++ repli xs n
  where
    repli' a 1 = [a]
    repli' a m = a : repli' a (m -1)

-- 16) Drop every N'th element from a list
dropEvery :: [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery xs n = dropEvery' 1 n xs
  where
    dropEvery' :: Int -> Int -> [a] -> [a]
    dropEvery' _ _ [] = []
    dropEvery' m coef (y : ys) =
      if m /= coef
        then y : dropEvery' (m + 1) coef ys
        else dropEvery' 1 coef ys

-- 17) Split a list into two parts; the length of the first part is given.
--     Do not use any predefined predicates.
split :: [a] -> Int -> ([a], [a])
split [] _ = ([], [])
split xs n = split' [] xs n
  where
    split' acc rest 0 = (acc, rest)
    split' acc (y : ys) m = split' (acc ++ [y]) ys (m - 1)

-- 18) Extract a slice from a list.
--     Given two indices, i and k, the slice is the list containing the elements between the i'th and k'th element of the original list
--     (both limits included). Start counting the elements with 1.
slice :: [a] -> Int -> Int -> [a]
slice [] _ _ = []
slice (x : xs) 1 n =
  if n == 0
    then []
    else x : slice xs 1 (n - 1)
slice (_ : xs) f l = slice xs (f - 1) (l - 1)

-- 19) Rotate a list N places to the left.
rotate :: [a] -> Int -> [a]
rotate [] _ = []
rotate xs 0 = xs
rotate (x : xs) n
  | n < 0 = rotate (xs ++ [x]) (n + 1)
  | otherwise = rotate xs (n - 1) ++ [x]

-- 20) Remove the K'th element from a list. (iteration from 0)
removeAt :: Int -> [a] -> [a]
removeAt _ [] = error "list's length less then removed element's index"
removeAt n (x : xs)
  | n == 0 = xs
  | otherwise = x : removeAt (n - 1) xs

-- 20) Remove the K'th element from a list. (iteration from 0)
--     changed returned value
removeAt' :: Int -> [a] -> (Maybe a, [a])
removeAt' _ [] = (Nothing, [])
removeAt' 0 (x : xs) = (Just x, xs)
removeAt' n (x : xs) =
  let (a, bs) = removeAt' (n - 1) xs
   in (a, x : bs)

-- 21) Insert an element at a given position into a list.
insertAt :: a -> [a] -> Int -> [a]
insertAt x xs 0 = x : xs
insertAt _ [] n
  | n > 0 = error "index is too big"
  | otherwise = error "index is less then 0"
insertAt a (x : xs) n = x : insertAt a xs (n - 1)

-- 22) Create a list containing all integers within a given range.
range :: Integer -> Integer -> [Integer]
range l r
  | r == l = [r]
  | r < l = l : range (l - 1) r
  | otherwise = l : range (l + 1) r

-- 23) Extract a given number of randomly selected elements from a list.
rndSelect :: [a] -> Int -> IO [a]
rndSelect [] _ = return []
rndSelect ls size
  | size < 0 = error "N must be greater than zero."
  | otherwise = do
    positions <- replicateM size $ getStdRandom $ randomR (0, length ls - 1)
    return . map (ls !!) $ positions

-- 24) Lotto: Draw N different random numbers from the set 1..M.
diffSelect :: Int -> Int -> IO [Int]
diffSelect size right
  | size == 0 = return []
  | size < 0 = error "size less then 0"
  | otherwise = replicateM size $ getStdRandom $ randomR (1, right)

-- 25) Generate a random permutation of the elements of a list.
rndPermutation :: [a] -> IO [a]
rndPermutation [] = return []
rndPermutation [x] = return [x]
rndPermutation xs = do
  randIdx <- randomRIO (0, length xs)
  let num = xs !! randIdx
  let (left, right) = split'' xs randIdx
  rndLeft <- rndPermutation left
  rndRight <- rndPermutation right
  return $ num : (rndLeft ++ rndRight)
  where
    split'' :: [a] -> Int -> ([a], [a])
    split'' (_ : xs) 0 = ([], xs)
    split'' (x : ls) n =
      let (ys, zs) = split'' ls (n - 1)
       in (x : ys, zs)

-- 26) Generate the combinations of K distinct objects chosen from the N elements of a list
-- TODO - understand!! (cheating :)))
combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations n xs =
  [xs !! idx : ls | idx <- [0 .. length xs - 1], ls <- combinations (n - 1) (drop (idx + 1) xs)]

-- 28) Sorting a list of lists according to length of sublists
-- a) We suppose that a list contains elements that are lists themselves.
--    The objective is to sort the elements of this list according to their length.
--    E.g. short lists first, longer lists later, or vice versa.
lsort :: [[a]] -> [[a]]
lsort [] = []
lsort (x : xs) = lsort [a | a <- xs, length a <= length x] ++ [x] ++ lsort [b | b <- xs, length b > length x]

-- TODO
-- b) Again, we suppose that a list contains elements that are lists themselves.
--    But this time the objective is to sort the elements of this list according to their length frequency;
--    i.e., in the default, where sorting is done ascendingly, lists with rare lengths are placed first,
--    others with a more frequent length come later.
lfsort :: [[a]] -> [[a]]
lfsort [] = []
lfsort (_ : _) = undefined

-- 31) Determine whether a given integer number is prime.
isPrime :: Integer -> Bool
isPrime 1 = True
isPrime x = isPrimeIn 2 x
  where
    isPrimeIn :: Integer -> Integer -> Bool
    isPrimeIn start n = (start >= n) || ((mod n start /= 0) && isPrimeIn (start + 1) n)

-- 32) Determine the greatest common divisor of two positive integer numbers. Use Euclid's algorithm.
gcd' :: Integral a => a -> a -> a
gcd' x y
  | x == y = x
  | x < y = gcd' x (y - x)
  | otherwise = gcd' (x - y) x

-- 33) Determine whether two positive integer numbers are coprime. Two numbers are coprime if their greatest common divisor equals 1.
coprime :: Integral a => a -> a -> Bool
coprime x y = gcd x y == 1

-- 34) Calculate Euler's totient function phi(m).
--      Euler's so-called totient function phi(m) is defined as the number of positive integers r (1 <= r < m) that are coprime to m.
--      Example: m = 10: r = 1,3,7,9; thus phi(m) = 4. Note the special case: phi(1) = 1.
-- totient n = length [x | x <- [1..n], coprime x n] -- or... ))
totient :: Int -> Int
totient n = length $ filter (coprime n) [1 .. n - 1]

-- 35) Determine the prime factors of a given positive integer.
--     Construct a flat list containing the prime factors in ascending order.
primeFactors :: Int -> [Int]
primeFactors = helper 2
  where
    helper :: Int -> Int -> [Int]
    helper start x
      | start > x = []
      | start == x = [x]
      | mod x start == 0 = (start :) $ helper start (div x start)
      | otherwise = helper (start + 1) x
