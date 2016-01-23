import Control.Applicative ((<$>))
import Control.Monad (forM_)
import Data.Array
import Debug.Trace
import Text.Printf

main = do
    t <- readLn :: IO Int
    forM_ [1..t] $ \i -> do
        [a,b,c] <- words <$> getLine
        let n = read a :: Int
            k = read b :: Int
            p = read c :: Double
        printf "Case #%d: %.9f\n" i (solve n k p)

solve :: Int -> Int -> Double -> Double
solve n k p =
    let bs = ((0,0), (n, n))
        chooseArray = listArray bs $ map (\(a,b) -> choose a b) $ range bs :: Array (Int, Int) Int
        choose :: Int -> Int -> Int
        choose n 0 = 1
        choose 0 k = 0
        choose n k = (chooseArray ! (n-1, k-1)) * n `div` k
        bests = listArray (0, n) $ map best [0..n]
        best !n
            | n >= k = maximum $ map (\i -> (expected i) + (bests ! (n-i))) [k..n]
            | otherwise = 0
        expected n = sum $ map (\i -> (fromIntegral $! chooseArray ! (n,i)) * (p**(fromIntegral i)) * (p**(fromIntegral $! n-i))) [k..n]
    in best n
