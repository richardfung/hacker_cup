import Control.Applicative ((<$>))
import Control.Monad (forM_)
import Text.Printf

main = do
    t <- readLn :: IO Int
    forM_ [1..t] $ \i -> do
        n <- readLn :: IO Int
        ds <- map read <$> words <$> getLine :: IO [Int]
        printf "Case #%d: %d\n" i $ count ds 0 0 n

count :: [Int] -> Int -> Int -> Int -> Int
count [x] _ acc n = acc + ((4 - ((n + acc) `mod` 4)) `mod` 4)
count (a:b:rest) p acc n
    | p == 3 = count (b:rest) 0 acc n
    | b <= a = count (b:rest) 0 (acc+3-p) n
    | b - a > 10 * (3-p) = count (b:rest) 0 (acc+3-p) n
    | b - a > 10 = let x = ceiling $ (fromIntegral $ b-a :: Double)/10
                   in count (b:rest) ((p+x) `mod` 4) (acc+x-1) n
    | otherwise = count (b:rest) (p+1 `mod` 4) acc n
