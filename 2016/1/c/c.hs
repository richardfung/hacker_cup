import Control.Applicative ((<$>))
import Control.Monad (forM_)
import Debug.Trace
import Text.Printf

main = do
    t <- readLn :: IO Int
    forM_ [1..t] $ \i -> do
        [n,a,b] <- map read <$> words <$> getLine :: IO [Int]
        cs <- map read <$> words <$> getLine :: IO [Int]
        printf "Case #%d: %.9f\n" i $ solve a b cs
    

solve :: Int -> Int -> [Int] -> Double
solve a b cs =
    let c = sum cs
        m = c * (ceiling $ (fromIntegral a) / (fromIntegral c))
    in if b > m then big a b cs
       else small a b cs

big :: Int -> Int -> [Int] -> Double
big a b cs =
    let c = sum cs
        left :: Int -> Int -> [Int] -> Double
        left a b cs =
            let d = a `div` c
                a' = a `mod` c
                helper :: Int -> Int -> [Int] -> Int
                helper a b (c:cs) =
                    if a >= c then helper (a-c) (b-c) cs
                    else (end a b (c:cs)) + (sum $ map (^2) cs)
            in (fromIntegral $ helper a' b cs) / (2*(fromIntegral $ b-a))
        middle :: [Int] -> Double
        middle cs =
            (fromIntegral $ sum $ map (^2) cs) / (2 * (fromIntegral c))
        right :: Int -> Int -> [Int] -> Double
        right a b cs =
            let b' = b `mod` c
                helper :: Int -> [Int] -> Int -> Int
                helper b (c:cs) acc =
                    if b >= c then helper (b-c) cs (acc + c^2)
                    else end 0 b (c:cs) + acc
            in (fromIntegral $ helper b' cs 0) / (2 * (fromIntegral $ b-a))
        leftEnd = c * (ceiling $ (fromIntegral a) / (fromIntegral c))
        rightStart = b - (b `mod` c)
        leftP = (fromIntegral $ leftEnd - a) / (fromIntegral $ b-a)
        rightP = (fromIntegral $ b - rightStart) / (fromIntegral $ b-a)
        middleP = 1 - leftP - rightP
        leftTerm = if leftP > 0 then (left a leftEnd cs)*leftP
                   else 0
        middleTerm = if middleP > 0 then (middle cs)*middleP
                     else 0
        rightTerm = if rightP > 0 then (right rightStart b cs)*rightP
                    else 0
    in leftTerm + middleTerm + rightTerm

end :: Int -> Int -> [Int] -> Int
end a b (c:cs)
    | a >= c = end (a-c) b cs
    | b >= c = (a+c)*(c-a)
    | otherwise = (b+a)*(b-a)

small :: Int -> Int -> [Int] -> Double
small a b cs =
    let left :: Int -> Int -> [Int] -> Int
        left a b (c:cs) = if a > c then left (a-c) (b-c) cs
                          else let r = if b-c > 0 then rest (b-c) cs 0
                                       else 0
                               in end a b (c:cs) + r
        rest :: Int -> [Int] -> Int -> Int
        rest b (c:cs) acc = if b > c then rest (b-c) cs (acc + c^2)
                            else acc + end 0 b (c:cs)
        c = sum cs
        d = a `div` c
        a' = a - c*d
        b' = b - c*d
    in (fromIntegral $ left a' b' cs) / (2*(fromIntegral $ b-a))
