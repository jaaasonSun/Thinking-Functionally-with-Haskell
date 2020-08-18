import Data.Char
import Data.List
import Data.Maybe

-- Exercise E
isqrt :: Float -> Integer
isqrt f = fst $ (shrink f) (bound f)
  where bound f = (upper `div` 2, upper)
          where upper = until ((> f) . (^2) . fromInteger) (*2) (1)
        shrink f (l, u)
          | l + 1 >= u = (l, u)
          | f >= fromInteger (m ^ 2) = shrink f (m, u)
          | otherwise = shrink f (l, m)
          where m = (l + u) `div` 2

-- Exercise F
sqrt_ :: (Floating a, Ord a) => a -> a
sqrt_ x = until (\y -> (abs (y ^ 2 - x)) < x * 1e-4) (\y -> (y + x / y) / 2) 1

-- Exercise G
data Nat = Zero | Succ Nat deriving (Eq, Show)
instance Num Nat where
  m - Zero = m
  Zero - Succ n = Zero
  Succ m - Succ n = m - n
  fromInteger x
    | x <= 0 = Zero
    | otherwise = Succ $ fromInteger $ x - 1

instance Ord Nat where
  Zero < Zero = False
  Zero < Succ m = True
  Succ m < Zero = False
  Succ m < Succ n = m < n

divMod_ :: Nat -> Nat -> (Nat, Nat)
divMod_ m n = if m < n then (Zero, m) else (Succ q, r)
  where (q, r) = divMod_ (m - n) n
