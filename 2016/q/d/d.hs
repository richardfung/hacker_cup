import Control.Monad.ST
import Data.Array.ST
import Data.STRef

data STNode s = STNode (STArray s Char (STNode s)) (STRef s Bool) | Empty

main = do
    print "Why is this so hard?"

genSTNode :: String -> STNode s -> ST s ()
genSTNode [] _ = return ()
{-
genSTNode [c] Empty 
genSTNode (c:cs) node = do
    
    -}
