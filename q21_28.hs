-- PROBLEM 21 --
insertAt :: Char -> String -> Int -> String
insertAt x xs n = take (n-1) xs ++ [x] ++ drop (n-1) xs

-- PROBLEM 22 --
range :: Int -> Int -> [Int]
range n m
  | n == m = [m]
  | n < m  = n:range (n+1) m

-- PROBLEM 23 --
import System.Random

rnd_select :: [a] -> Int -> IO [a]
-- TODO