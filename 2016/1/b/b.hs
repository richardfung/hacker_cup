import Control.Applicative ((<$>))
import Control.Monad (forM_)
import Data.Map as M
import Data.Set as S
import Debug.Trace
import Text.Printf

main = do
    t <- readLn :: IO Int
    forM_ [1..t] $ \i -> do
        [l, n, m, d] <- Prelude.map read <$> words <$> getLine :: IO [Int]
        ws <- Prelude.map read <$> words <$> getLine :: IO [Int]
        printf "Case #%d: %d\n" i $ solve l ws m d

solve :: Int -> [Int] -> Int -> Int -> Int
solve l ws m d =
    let washSet = Prelude.foldr S.insert S.empty
                  $ Prelude.map (\(a,id') -> (a,a,id')) $ zip ws [1..]
        dryMap = M.singleton 0 m
    in dryTime (washTimes l washSet []) dryMap d

dryTime :: [Int] -> M.Map Int Int -> Int -> Int
dryTime [] dm _ = fst $ M.findMax dm
dryTime (t:ts) !dm d =
    let (tStart, count) = M.findMin dm
        k = max t tStart + d
        newDm = M.insert k (findWithDefault 0 k dm + 1)
            $ if count > 1 then M.insert tStart (count-1) dm else M.delete tStart dm
    in dryTime ts newDm d

washTimes :: Int -> S.Set (Int, Int, Int) -> [Int] -> [Int]
washTimes 0 ws acc = reverse acc
washTimes loads !ws acc =
    let (a,b,id') = S.findMin ws
    in washTimes (loads-1) (S.insert (a+b, b, id') $ S.delete (a, b, id') ws)
                 (a:acc)

traceShowId x = traceShow x x
