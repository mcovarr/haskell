
module Reverse where

import Data.Foldable

-- tail recursive reverse helper
rvrs' :: [a] -> [a] -> [a]
rvrs' [] acc = acc
rvrs' (x:xs) acc = rvrs' xs $ [x] ++ acc

-- reverse a list
rvrs :: [a] -> [a]
rvrs xs = rvrs' xs []

-- predicate for a character being a space
space :: Char -> Bool
space ' ' = True
space _ = False

-- split' has two accumulators: the first for the list currently under construction (lacc),
-- the second for the list of lists under construction (llacc).

-- break up a list of things into a list of lists of things according to a predicate.
-- the last two arguments are the current list acc and the overall list of list acc
split' :: [a] -> (a -> Bool) -> [a] -> [a] -> [[a]] -> [[a]]

split' list predicate zero = split'' list 
  where -- no more elements and an empty current list accumulator
        split'' [] [] llacc = llacc
        -- no more elements and a nonempty current list accumulator
        split'' [] lacc llacc = llacc ++ [lacc]
        -- more elements and the current element satisfies the predicate
        split'' (x:xs) lacc llacc | predicate x = split'' xs zero (llacc ++ [lacc])
        -- more elements and the current element does not satisfy the predicate
        split'' (x:xs) lacc llacc = split'' xs (lacc ++ [x]) llacc

split :: String -> [String]
split x = split' x space "" "" []

join' :: (Foldable t, Eq a) => [a] -> [a] -> t [a] -> [a]
join' e j = foldl fn e where
    fn x y | x == e    = y
           | otherwise = x ++ j ++ y

run :: String -> String
run = (join' "" " ") . rvrs . split

