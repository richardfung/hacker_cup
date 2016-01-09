import Control.Applicative ((<$>))
import Control.Monad (forM_)
import Data.Array.Unboxed
import Text.Printf
import Debug.Trace

main = do
    t <- readLn :: IO Int
    forM_ [1..t] $ \i -> do
        [n, p] <- map read <$> words <$> getLine :: IO [Int]
        let toArray :: [Int] -> UArray Int Int
            toArray xs = listArray (0, length xs - 1) xs
        boxes <- toArray <$> map read <$> words <$> getLine
            :: IO (UArray Int Int)
        printf "Case #%d: %d\n" i $ solve p boxes

solve :: Int -> UArray Int Int -> Int
solve p boxes = sum $ map (simple p boxes) $ range $ bounds boxes

simple :: Int -> UArray Int Int -> Int -> Int
simple p boxes start = helper start 0 0
    where
          helper pos s acc
              | not $ inRange (bounds boxes) pos = acc
              | newS > p = acc
              | otherwise = helper (pos+1) newS (acc+1)
              where newS = s + boxes ! pos
