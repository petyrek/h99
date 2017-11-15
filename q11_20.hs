import Data.List

data Entry a = Single a | Multiple Int a
  deriving (Show)

-- PROBLEM 11 --
encodeModified :: Eq a => [a] -> [Entry a]
encodeModified = map getEntry . group
    where
      getEntry x = if length x == 1
               then Single (head x)
               else Multiple (length x) (head x)

-- PROBLEM 12 --
decodeModified :: [Entry a] -> [a]
decodeModified = concatMap decodeHelper
    where
      decodeHelper (Single a) = [a]
      decodeHelper (Multiple n a) = replicate n a

-- PROBLEM 13 --
encodeDirect :: Eq a => [a] -> [Entry a]
encodeDirect [] = []
encodeDirect (x:xs) = encode (x : takeWhile (==x) xs) : encodeDirect (dropWhile (==x) xs)
      where encode [x]    = Single x
            encode (x:xs) = Multiple (length xs + 1) x

-- PROBLEM 14 --
dupli :: [a] -> [a]
dupli = concatMap(\x -> [x, x])

-- PROBLEM 15 --
repli :: [a] -> Int -> [a]
repli xs n = concatMap (replicate n) xs

-- PROBLEM 16 --
dropEvery :: [a] -> Int -> [a]
dropEvery xs n = helper xs n
      where helper [] _ = []
            helper (x:xs) 1 = helper xs n
            helper (x:xs) k = x:helper xs (k-1)

-- PROBLEM 17 --
split :: [a] -> Int -> ([a], [a])
split xs n = (take n xs, drop n xs)

-- PROBLEM 18 --
slice :: [a] -> Int -> Int -> [a]
slice xs i k = take (k-i+1) (drop (i-1) xs)

-- PROBLEM 19 --
rotate :: [a] -> Int -> [a]
rotate xs n = if n > 0 then rotateRight xs n else rotateLeft xs (-n)
      where rotateRight xs n = drop n xs ++ take n xs
            rotateLeft  xs n = drop (length xs - n) xs ++ take (length xs - n) xs

-- PROBLEM 20 --
removeAt :: Int -> [a] -> (a, [a])
removeAt n xs = (xs !! (n-1), take (n-1) xs ++ drop n xs)