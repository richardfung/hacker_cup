import Control.Monad (forM, forM_)
import Control.Monad.ST
import Data.Array
import Data.Array.ST
import Data.STRef

data Node = Node (Array Char Node) Bool | Empty
data STNode s = STNode (STArray s Char (STNode s)) (STRef s Bool) | STEmpty

main = do
    print "Why is this so hard?"

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
