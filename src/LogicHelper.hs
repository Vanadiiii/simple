module LogicHelper (module LogicHelper) where

and2 :: Bool -> Bool -> Bool
and2 True True = True
and2 _ _ = False

or2 :: Bool -> Bool -> Bool
or2 False False = False
or2 _ _ = True

not2 :: Bool -> Bool
not2 True = False
not2 False = True

nand2 :: Bool -> Bool -> Bool
nand2 a b = not2 $ and2 a b

xor2 :: Bool -> Bool -> Bool
xor2 True True = False
xor2 x y = or2 x y

impl2 :: Bool -> Bool -> Bool
impl2 a b = not2 a `or2` b

eq2 :: Bool -> Bool -> Bool
eq2 a b = a == b

-- 46) Define predicates and/2, or/2, nand/2, nor/2, xor/2, impl/2 and equ/2 (for logical equivalence) which succeed or fail according to the result of their respective operations; e.g. and(A,B) will succeed, if and only if both A and B succeed.
--     A logical expression in two variables can then be written as in the following example: and(or(A,B),nand(A,B)).
--     Now, write a predicate table/3 which prints the truth table of a given logical expression in two variables.
table :: (Bool -> Bool -> Bool) -> IO ()
table f = do
  printRow True True
  printRow True False
  printRow False True
  printRow False False
    where
      printRow a b = print $ show a ++ " " ++ show b ++ " " ++ show (f a b)
