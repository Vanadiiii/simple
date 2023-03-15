module MathHelper (module MathHelper) where

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
