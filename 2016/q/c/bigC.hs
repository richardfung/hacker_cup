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
solve p boxes = helper 0 0 (boxes ! 0) 0
    where
          -- helper returns number of subsequences in [start, end of boxes]
          helper :: Int -> Int -> Int -> Int -> Int
          helper start currentEnd currentSum acc
              | not $ inRange (bounds boxes) start = acc
              | otherwise =
                  let (newEnd, newSum) = findEnd p boxes currentEnd currentSum
                      newAcc = acc + (newEnd - start)
                      newStart = start+1
                  in if newEnd == start then
                         -- we have to move end with start
                         helper newStart (newEnd+1) (boxes ! (newEnd+1)) newAcc
                     else helper newStart newEnd (newSum - (boxes ! start)) newAcc

-- returns (end, currentSum) where end is 1 past where it can go
findEnd :: Int -> UArray Int Int -> Int -> Int -> (Int, Int)
findEnd p boxes end currentSum =
    if not (inRange (bounds boxes) end) || currentSum > p then (end, currentSum)
    else findEnd p boxes (end+1) $ currentSum + (boxes ! (end + 1))
