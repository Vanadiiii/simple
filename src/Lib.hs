module Lib
  ( someFunc,
    last,
    lastButOne,
  )
where

import Prelude hiding (last)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- Find the last element of a list
last :: [a] -> a
last [] = error "empty list"
last [x] = x
last (_ : xs) = last xs

--Find the last but one element of a list.
lastButOne :: [a] -> a
lastButOne [] = error "list is empty"
lastButOne [_] = error "list's size is less then 2"
lastButOne (x : [_]) = x
lastButOne (_ : xs) = lastButOne xs
