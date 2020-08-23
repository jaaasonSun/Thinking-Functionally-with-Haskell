import Data.List

-- Exercise B
length1 :: [a] -> Integer
length1 = length1' 0
  where length1' n [] = n
        length1' n (x:xs) = m `seq` length1' m xs
          where m = n + 1

length2 :: [a] -> Integer
length2 = length2' 0
  where length2' n [] = n
        length2' n (x:xs) = if n == 0 then length2' 1 xs else length2' (n+1) xs

        
-- Exercise J
select :: Ord a => Int -> [a] -> a
select 0 [x] = x
select k (x:xs)
  | k == len = x
  | k > len = select (k - len - 1) l
  | otherwise = select k s
  where (s, l) = partition (<= x) xs
        len = length s
