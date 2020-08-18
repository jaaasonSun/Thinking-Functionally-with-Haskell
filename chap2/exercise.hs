import Data.Char (toUpper, toLower, isAlpha)
import Data.List (words)
import Data.Maybe
import Text.Read (read)

-- Exercise C
modernise :: String -> String
modernise = unwords . map (\xs -> ((\x -> (if isAlpha x then toUpper x else x)) $ head xs) : (tail xs)) . words

-- Exercise D
first :: (p -> Bool) -> [p] -> p
first p xs
  | null xs = error "Empty list"
  | p x = x
  | otherwise = first p $ tail xs
  where x = head xs

-- Exercise E
first_ :: (p -> Bool) -> [p] -> Maybe p
first_ p xs
  | null ys = Nothing
  | otherwise = Just (head ys)
  where ys = filter p xs

-- Exercise F
exp_ :: Integer -> Integer -> Integer
exp_ x n
  | n == 0 = 1
  | n == 1 = x
  | even n = let half = exp_ x (n `div` 2) in half * half 
  | odd n = x * exp_ x (n - 1)

-- Exercise G
type Date = (Int, Int, Int)
showDate :: Date -> String
showDate (d, m, y) = (show d) ++ (suffix d) ++ " " ++ (name !! m) ++ ", " ++ (show y)
  where suffix d
          | last >= 1 && last <= 3 = suf !! last
          | otherwise = head suf
          where suf = ["th", "st", "nd", "rd"]
                last = (d `mod` 10)
        name = ["", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]

-- Exercise H
type CIN = [Char]
vaild :: [Char] -> Bool
vaild cs = getSum cs == getCheck cs
  where getSum = sum . map (\c -> read [c] :: Int) . take 8
        getCheck cs = read [cs !! 8, cs !! 9] :: Int

-- Exercise I
palindrom :: IO ()
isPalin :: [Char] -> Bool
isPalin cs = fs == reverse fs
  where fs = map toLower $ filter isAlpha cs
palindrom = do {
  putStrLn "Enter a string:";
  str <- getLine;
  putStrLn (if isPalin str then "Yes!" else "No!");
  }
