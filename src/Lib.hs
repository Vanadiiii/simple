module Lib
  ( someFunc,
    myLast,
  )
where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

myLast :: [a] -> a
myLast [] = error "empty list"
myLast [x] = x
myLast (_ : xs) = myLast xs
