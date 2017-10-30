import Data.List

myLast :: [a] -> a
myLast [x] = x
myLast (x:xs) = myLast xs

myButLast :: [a] -> a
myButLast (x:xs) = if length xs == 1 then x
                   else myButLast xs

elementAt :: [a] -> Int -> a
elementAt (x:xs) 1 = x
elementAt (x:xs) n = elementAt xs (n - 1)

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

isPalindrome :: Eq a => [a] -> Bool
isPalindrome [] = True
isPalindrome [_] = True
isPalindrome (x:xs) = x == last xs && isPalindrome (init xs)

data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (Elem a) = [a]
flatten (List (x:xs)) = flatten x ++ flatten (List xs)
flatten (List []) = []

compress :: Eq a => [a] -> [a]
compress []     = []
compress (x:xs) = x : (compress $ dropWhile (== x) xs)

encode :: Eq a => [a] -> [(Int, a)]
encode = map (\x -> (length x, head x)) . group