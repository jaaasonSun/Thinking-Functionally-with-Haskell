-- Exercise B
cycle_ :: [a] -> [a]
cycle_ [] = error "empty list"
cycle_ xs = concat xss where xss = xs:xss

-- Exercise C
fibs :: [Integer]
fibs = 0:1:fibs' where fibs' = zipWith (+) fibs (tail fibs) 

-- Exercise D
merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
  | x < y = x:merge xs (y:ys)
  | x == y = x:merge xs ys
  | x > y = y:merge (x:xs) ys

xmerge :: Ord a => [a] -> [a] -> [a]
xmerge (x:xs) ys = x:merge xs ys

hamming :: [Int]
hamming = 1:hamming'
  where hamming' = foldr xmerge [] $ map expand hamming
          where expand n = [2*n, 3*n, 5*n]

-- Exercise L
type Matrix a = [[a]]
data Torus a = Cell a (Torus a) (Torus a) (Torus a) (Torus a)
elem_ (Cell a u d l r) = a
up :: Torus a -> Torus a
up (Cell a u d l r) = u
down (Cell a u d l r) = d
left (Cell a u d l r) = l
right (Cell a u d l r) = r

rotr xs = [last xs] ++ init xs
rotl xs = tail xs ++ [head xs]

zipWith5 :: (t -> a1 -> a2 -> a3 -> a4 -> a5) -> [t] -> [a1] -> [a2] -> [a3] -> [a4] -> [a5]
zipWith5 f (a:as) bs cs ds es
  = f a (head bs) (head cs) (head ds) (head es)
    : zipWith5 f as (tail bs) (tail cs) (tail ds) (tail es)
zipWith5 _ _ _ _ _ _ = []

mkTorus :: Matrix a -> Torus a
mkTorus ass = head $ head mat
  where mat = zipWith5 (zipWith5 Cell) ass (rotr mat) (rotl mat) (map rotr mat) (map rotl mat) 
