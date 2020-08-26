import Control.Monad
import System.IO
import Control.Monad.ST
import Data.STRef

-- Exercise B
add31 :: Num a => Maybe a -> Maybe a -> Maybe a -> Maybe a
add31 (Just a) (Just b) (Just c) = Just $ a + b + c
add31 _ _ _ = Nothing

add32 :: (Monad m, Num b) => m b -> m b -> m b -> m b
add32 ma mb mc =
  do {
    a <- ma;
    b <- mb;
    c <- mc;
    return (a + b + c)
    }

-- Exercise E
sequence' :: Monad m => [m a] -> m [a]
sequence' = foldr f (return [])
  where f mx mxs = do { x <- mx; xs <- mxs; return (x:xs) }


mapM' :: Monad m => (a1 -> m a2) -> [a1] -> m [a2]
mapM' f = sequence' . map f

mapM_' :: Monad m => (a1 -> m a2) -> [a1] -> m ()
mapM_' f = sequence_ . map f

foldM' :: Monad m => (b -> a -> m b) -> b -> [a] -> m b
foldM' f e [] = return e
foldM' f e (x:xs) = (f e x) >>= (\y -> foldM' f y xs)

forM_' :: Monad m => [a] -> (a -> m b) -> m ()
forM_' = flip mapM_'

-- Exercice K
repeatFor :: Monad m => Int -> m a1 -> m (a2 -> a2)
repeatFor n = foldr (>>) (return id) . replicate n

fibST :: Int -> ST s (Integer, Integer)
fibST n = do { a <- newSTRef (0, 1);
               repeatFor n
               (do { (x, y) <- readSTRef a;
                     x `seq` (x + y) `seq` writeSTRef a (y, x + y)});
               readSTRef a}
  
fib :: Int -> Integer
fib n = fst $ runST (fibST n)
