import Control.Applicative ((<$>))
import Control.Monad (forM_)
import Data.List (sort)
import Data.Map as M
import Data.Maybe
import Text.Printf

main = do
    t <- readLn :: IO Int
    forM_ [1..t] $ \i -> do
        n <- readLn :: IO Int
        let readTuple [a,b] = (read a, read b)
        ladders <- mapM (\_ -> readTuple <$> words <$> getLine) [1..n] :: IO [(Int, Int)]
        printf "Case #%d: %d\n" i (solve $! sort ladders)

solve :: [(Int, Int)] -> Int
solve ladders = count ladders M.empty

count :: [(Int, Int)] -> M.Map Int [Int] -> Int
count [] _ = 0
count ((x,h):ls) !acc =
    let newAcc = M.insert h (x:(findWithDefault [] h acc)) $! removeSmaller acc h
        add = if M.member h acc then
                  (sum $! Prelude.map (\n -> (x - n)^2 ) (acc ! h))
              else 0
    in (count ls newAcc + add) `mod` (10^9 + 7)

removeSmaller :: M.Map Int [Int] -> Int -> M.Map Int [Int]
removeSmaller !m !n =
    case M.lookupLT n m of Just (h,x) -> removeSmaller (delete h m) n
                           Nothing -> m
