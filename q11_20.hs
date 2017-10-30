import Data.List

data Entry a = Single a | Multiple Int a
  deriving (Show)

encodeModified :: Eq a => [a] -> [Entry a]
encodeModified = map getEntry . group
    where
      getEntry x = if length x == 1
               then Single (head x)
               else Multiple (length x) (head x)


decodeModified :: [Entry a] -> [a]
decodeModified = concatMap decodeHelper
    where
      decodeHelper (Single a) = [a]
      decodeHelper (Multiple n a) = replicate n a