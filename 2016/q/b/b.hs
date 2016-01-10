import Control.Applicative ((<$>), (<*>))
import Control.Monad (forM_)
import Debug.Trace
import Text.Printf

main = do
    t <- readLn :: IO Int
    forM_ [1..t] $ \i -> do
        getLine
        rows <- zip <$> getLine <*> getLine
        printf "Case #%d: %d\n" i $ solve rows 

solve :: [(Char, Char)] -> Int
solve rows = helper $ toInts rows
    where
          helper :: [(Int,Int)] -> Int
          helper ((0,0):[]) = 0
          helper ((0,_):[]) = 1
          helper ((_,0):[]) = 1
          helper ((1,_):[]) = 1
          helper ((_,1):[]) = 1
          helper ((_,_):[]) = 2
          helper ((0,0):(0,0):rest) = helper $ (0,0):rest
          helper ((0,_):(0,0):rest) = helper ((0,0):rest) + 1
          helper ((_,0):(0,0):rest) = helper ((0,0):rest) + 1
          helper ((1,_):(0,0):rest) = helper ((0,0):rest) + 1
          helper ((_,1):(0,0):rest) = helper ((0,0):rest) + 1
          helper ((_,_):(0,0):rest) = helper ((0,0):rest) + 2
          helper ((0,_):(0,b):rest) = helper ((0,b):rest)
          helper ((1,0):(0,b):rest) = helper ((0,b):rest) + 1
          helper ((1,_):(0,b):rest) = helper (seeRight $ (0,b):rest) + 1
          helper ((_,_):(0,b):rest) = helper ((0,b):rest) + 1
          helper ((_,0):(a,0):rest) = helper ((a,0):rest)
          helper ((0,1):(a,0):rest) = helper ((a,0):rest) + 1
          helper ((_,1):(a,0):rest) = helper (seeLeft $ (a,0):rest) + 1
          helper ((_,_):(a,0):rest) = helper ((a,0):rest) + 1
          helper ((_,_):(a,b):rest) = helper ((a,b):rest)

seeLeft :: [(Int, Int)] -> [(Int, Int)]
seeLeft xs =
    let test (a,b) = a /= 0
        lefts = takeWhile test xs
        newLefts = map (\(_,b) -> (0,b)) lefts
    in newLefts ++ (dropWhile test xs)

seeRight :: [(Int, Int)] -> [(Int, Int)]
seeRight xs =
    let test (a,b) = b /= 0
        rights = takeWhile test xs
        newRights = map (\(a,_) -> (a,0)) rights
    in newRights ++ (dropWhile test xs)

toInts :: [(Char, Char)] -> [(Int, Int)]
toInts rows = helper rows 0 0
    where
          helper [] _ _ = []
          helper ((a,b):rest) x y =
              let next 'X' _ = 0
                  next '.' i = i+1
                  nextX = next a x
                  nextY = next b y
              in (nextX,nextY):(helper rest nextX nextY)
