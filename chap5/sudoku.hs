module SudokuSolver (solve) where

type Row a = [a]
type Matrix a = [Row a]
type Grid = Matrix Digit
type Digit = Char

solve :: Grid -> [Grid]
solve = search . choices
  where
    search cm
      | not (safe pm) = []
      | complete pm = [map (map head) pm]
      | otherwise = concat (map search (expand1 pm))
      where pm = prune cm
        
choices :: Grid -> Matrix [Digit]
choices = map (map choice)
  where choice d = if d == '0' then ['1'..'9'] else [d]

expand1 :: Matrix [Digit] -> [Matrix [Digit]]
expand1 rows = [rows1 ++ [row1 ++ [c]:row2] ++ rows2 | c <- cs]
  where (rows1, row:rows2) = break (any smallest) rows
        (row1, cs:row2) = break smallest row
        smallest cs = length cs == n
        n = minimum (counts rows)
        counts = filter (/= 1) . map length . concat

complete :: Matrix [Digit] -> Bool
complete = all (all single)
  where single [_] = True
        single _ = False

safe :: Eq a => Matrix [a] -> Bool
safe m = all ok (rows m) && all ok (cols m) && all ok (boxs m)
  where ok row = nodups [x | [x] <- row]

rows :: Matrix a -> Matrix a
rows = id
cols :: Matrix a -> Matrix a
cols [xs] = [[x] | x <- xs]
cols (xs:xss) = zipWith (:) xs (cols xss)
boxs :: Matrix a -> Matrix a
boxs = map concat . concat . map cols . group . map group
  where group [] = []
        group xs = take 3 xs : group (drop 3 xs)

nodups :: (Eq a) => [a] -> Bool
nodups [] = True
nodups (x:xs) = all (/= x) xs && nodups xs



pruneRow :: Row [Digit] -> Row [Digit]
pruneRow row = map (remove fixed) row
  where fixed = [d | [d] <- row]
        remove ds [x] = [x]
        remove ds xs = filter (`notElem` ds) xs

prune :: Matrix [Digit] -> Matrix [Digit]
prune = pruneBy boxs . pruneBy rows . pruneBy cols
  where pruneBy f = f . map pruneRow . f


