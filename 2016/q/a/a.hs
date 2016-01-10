import Control.Applicative ((<$>))
import Control.Monad (forM_)
import Data.Map.Strict as M
import Debug.Trace
import Text.Printf

type Key = ((Int, Int), Int)

main = do
    t <- readLn :: IO Int
    forM_ [1..t] $ \i -> do
        n <- readLn :: IO Int
        let pair [x,y] = (x,y)
        stars <- mapM (\_ -> pair <$> Prelude.map read <$> words <$> getLine) [1..n]
            :: IO [(Int, Int)]
        printf "Case #%d: %d\n" i $ solve stars

solve :: [(Int, Int)] -> Int
solve stars =
    let m = genMap stars M.empty
    in sum $ Prelude.map (\n -> if n >= 2 then (n*(n-1)) `div` 2
                                else 0)
                         $ elems m

genMap :: [(Int, Int)] -> Map Key Int -> Map Key Int
genMap [] acc = acc
genMap (s:ss) acc =
    let keyPairs = Prelude.map (toKeys s) ss
    in genMap ss $
           Prelude.foldr (\(k0, k1) -> \m -> increment k0 $ increment k1 m)
                         acc keyPairs

increment :: Key -> M.Map Key Int -> M.Map Key Int
increment k m = M.insert k (1 + M.findWithDefault 0 k m) m

toKeys :: (Int, Int) -> (Int, Int) -> (Key, Key)
toKeys a@(x0,y0) b@(x1,y1) = let l = (x0-x1)^2 + (y0-y1)^2
                             in ((a,l), (b,l))
