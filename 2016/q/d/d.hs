import Control.Applicative ((<$>))
import Control.Monad (forM, forM_)
import Control.Monad.ST
import Data.Array
import Data.Array.ST
import Data.Maybe (fromJust)
import Data.STRef
import Debug.Trace
import Text.Printf

data Node = Node (Array Char Node) Bool | Empty deriving Show
data STNode s = STNode (STArray s Char (STNode s)) (STRef s Bool) | STEmpty

main = do
    t <- readLn :: IO Int
    forM_ [1..t] $ \i -> do
        [n,k] <- map read <$> words <$> getLine :: IO [Int]
        words' <- forM [1..n] $ \_ -> getLine
        printf "Case #%d: %d\n" i (fromJust $ lookup k $ count $ genNode words')

combine :: [[(Int, Int)]] -> [(Int, Int)]
combine [] = [(0,0)]
combine [x] = (0,0):x
combine (x:xs) =
    let rest = combine xs
        restArray = array (0, length rest-1) rest
        xArray = array (0, length x) $ (0,0):x
        max' = snd (bounds restArray) + snd (bounds xArray)
        wayToN n = minimum $ map (\i -> (xArray ! i) + (restArray ! (n-i))) [max 0 (n-(snd $ bounds restArray))..min n (snd $ bounds xArray)]
    in map (\i -> (i, wayToN i)) [0..max']

count :: Node -> [(Int, Int)]
count Empty = []
count (Node children isWord) =
    let childCounts = map count $ filter (\c -> case c of Empty -> False
                                                          otherwise -> True) $ elems children
        incrementedChildCounts = map (\c -> map (\(i,j) -> (i,j+2)) c) childCounts
        allCounts = if isWord then [(1,1)]:incrementedChildCounts
                    else incrementedChildCounts 
    in tail $ combine allCounts

genNode :: [String] -> Node
genNode ss = runST $ do
    stNode <- newSTNode
    forM_ ss $ \s -> do
        genSTNode s stNode
    stNodeToNode stNode

genSTNode :: String -> STNode s -> ST s ()
genSTNode [] _ = return ()
genSTNode (c:[]) (STNode stArray _) = do
    child <- readArray stArray c
    case child of STEmpty -> do nextBool <- newSTRef True
                                nextArray <- newArray ('a', 'z') STEmpty
                                writeArray stArray c $ (STNode nextArray nextBool)
                  STNode childArray stBool -> writeSTRef stBool True
genSTNode (c:cs) (STNode stArray _) = do
    child <- readArray stArray c
    case child of STEmpty -> do nextBool <- newSTRef False
                                nextArray <- newArray ('a', 'z') STEmpty
                                writeArray stArray c $ (STNode nextArray nextBool)
                                child <- readArray stArray c
                                genSTNode cs child
                  otherwise -> genSTNode cs child

newSTNode :: ST s (STNode s)
newSTNode = do
    retArray <- newArray ('a', 'z') STEmpty
    retBool <- newSTRef False
    return $ STNode retArray retBool

stNodeToNode :: STNode s -> ST s Node
stNodeToNode STEmpty = return Empty
stNodeToNode (STNode stArray stBool) = do
    bs <- getBounds stArray
    children <- forM (range bs) $ \i -> do
                    c <- readArray stArray i
                    stNodeToNode c
    retBool <- readSTRef stBool
    return $ Node (listArray bs children) retBool

traceShowId x = traceShow x x
