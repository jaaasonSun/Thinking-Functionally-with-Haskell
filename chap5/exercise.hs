import Data.List (sort)
import Data.Char (isSpace)

-- Exercise D
nodups :: Ord a => [a] -> Bool
nodups = nodups' . sort
  where nodups' [] = True
        nodups' [x] = True
        nodups' xs'@(x:xs) = not $ or $ zipWith (==) xs xs'

-- Exercise E
nub_ :: Ord a => [a] -> [a]
nub_ = nub' . sort
  where nub' (x:y:xs) = if x == y then nub' (y:xs) else x : nub' (y:xs)
        nub' xs = xs

-- Exercise F
takeWhile_ :: (a -> Bool) -> [a] -> [a]
takeWhile_ p [] = []
takeWhile_ p (x:xs) = if p x then x:takeWhile_ p xs else []

dropWhile_ :: (a -> Bool) -> [a] -> [a]
dropWhile_ p [] = []
dropWhile_ p (x:xs) = if p x then dropWhile_ p xs else x:xs

words_ :: [Char] -> [[Char]]
words_ [] = []
words_ xs = takeWhile_ (not . whiteSpace) startWithNotSpace :
  words_ (dropWhile_ (not . whiteSpace) startWithNotSpace)
  where whiteSpace = isSpace
        startWithNotSpace = dropWhile_ whiteSpace xs

-- Exercise G
minimum_ :: Ord a => [a] -> a
minimum_ [x] = x
minimum_ (x:xs) = x `min` minimum_ xs
