import Data.List

-- PROBLEM 1 --
myLast :: [a] -> a
myLast [x] = x
myLast (x:xs) = myLast xs

-- PROBLEM 2 --
myButLast :: [a] -> a
myButLast (x:xs) = if length xs == 1 then x
                   else myButLast xs

-- PROBLEM 3 --
elementAt :: [a] -> Int -> a
elementAt (x:xs) 1 = x
elementAt (x:xs) n = elementAt xs (n - 1)

-- PROBLEM 5 --
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

-- PROBLEM 6 --
isPalindrome :: Eq a => [a] -> Bool
isPalindrome [] = True
isPalindrome [_] = True
isPalindrome (x:xs) = x == last xs && isPalindrome (init xs)

-- PROBLEM 7 --
data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (Elem a) = [a]
flatten (List (x:xs)) = flatten x ++ flatten (List xs)
flatten (List []) = []

-- PROBLEM 8 --
compress :: Eq a => [a] -> [a]
compress []     = []
compress (x:xs) = x : (compress $ dropWhile (== x) xs)

-- PROBLEM 9 --
pack :: [a] -> [[a]]
pack (x:xs) = x:(takeWhile (==x) xs) : pack (dropWhile (==x) xs)


-- PROBLEM 10 --
encode :: Eq a => [a] -> [(Int, a)]
encode = map (\x -> (length x, head x)) . group