-- Exercise B
allPairs = [(x, sum - x) | sum <- [0..], x <- [0..sum]]

-- Exercise C
disjoint :: Ord a => [a] -> [a] -> Bool
disjoint [] _ = True
disjoint _ [] = True
disjoint xs'@(x:xs) ys'@(y:ys)
  | x == y = False
  | x < y = disjoint xs ys'
  | otherwise = disjoint xs' ys


-- Exercise E
essentiallyDifferent :: (Num d, Eq d, Enum d) => d -> [(d, d, d, d)]
essentiallyDifferent n = [(a, b, c, d) | a <- [1..n], b <- [a+1..n],
                           c <- [a+1..b-1], d <- [c..b-1],
                           a^3 + b^3 == c^3 + d^3]

data RList a = Nil | Snoc (RList a) a deriving (Show)

head' :: RList a -> a
head' (Snoc Nil x) = x
head' (Snoc xs x) = head' xs
last :: RList a -> a
last (Snoc xs x) = x

toList :: [a] -> RList a
toList = toList' . reverse
  where toList' [] = Nil
        toList' (x:xs) = Snoc (toList' xs) x

fromList :: RList a -> [a]
fromList = reverse . fromList'
  where fromList' Nil = []
        fromList' (Snoc xs x) = x : fromList' xs

-- Exercise H
splitAt' :: (Eq a1, Num a1) => a1 -> [a2] -> ([a2], [a2])
splitAt' 0 xs = ([], xs)
splitAt' _ [] = ([], [])
splitAt' n (x:xs) = (x:f, s)
  where (f, s) = splitAt' (n - 1) xs

-- Exercise L
fork :: (a -> b,a -> c) -> a -> (b,c)
fork (f,g) x = (f x, g x)

class Bifunctor p where
  bimap :: (a -> b) -> (c -> d) -> p a c -> p b d

instance Bifunctor Either where
  bimap f g (Left a) = Left (f a)
  bimap f g (Right b) = Right (g b)
