module BinaryTreeHelper (module BinaryTreeHelper) where

data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)

-- Problem 55
-- Construct completely balanced binary trees
-- TODO
cbalTree = undefined

-- Problem 56A
-- Symmetric binary trees
symmetric :: Eq a => Tree a -> Bool
symmetric Empty = True
symmetric (Branch _ left right) = helper left right
  where
    helper Empty Empty = True
    helper (Branch a l1 r1) (Branch b l2 r2) = a == b && helper l1 r2 && helper r1 l2
    helper _ _ = False

-- Problem 56B
-- We are only interested in the structure, not in the contents of the nodes.
symmetric2 :: Tree a -> Bool
symmetric2 Empty = True
symmetric2 (Branch _ left right) = helper left right
  where
    helper Empty Empty = True
    helper (Branch _ l1 r1) (Branch _ l2 r2) = helper l1 r2 && helper r1 l2
    helper _ _ = False

-- Problem 57
