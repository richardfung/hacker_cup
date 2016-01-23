import Control.Applicative ((<$>))
import Control.Monad (forM_)
import Data.Array.Unboxed
import Debug.Trace
import Text.Printf

main = do
    t <- readLn :: IO Int
    forM_ [1..t] $ \i -> do
        getLine
        as <- getLine
        bs <- getLine
        let aArray = listArray (0, length as - 1) as :: UArray Int Char
            bArray = listArray (0, length bs - 1) bs :: UArray Int Char
        printf "Case #%d: %d\n" i (solve aArray bArray)

solve :: UArray Int Char -> UArray Int Char -> Int
solve as bs =
    let minA = minimum $! map (\i -> max (count as bs i (\n -> n-1)) (count as bs (i+1) (+1))) $! indices as
        minB = minimum $! map (\i -> max (count bs as i (\n -> n-1)) (count bs as (i+1) (+1))) $! indices bs
    in min minA minB

count :: UArray Int Char -> UArray Int Char -> Int -> (Int -> Int) -> Int
count !as !bs !start !f =
    let p = moveTillNotMatch as bs start f
    in countSames as p (as ! p) f

-- counts number of times we have to paint assuming we want to look like as
countSames :: UArray Int Char -> Int -> Char -> (Int -> Int) -> Int
countSames !as !p c f =
    if inRange (bounds as) p then
        let newP = moveTillNotSame as p c f
        in 1 + (countSames as newP (as ! newP) f)
    else 0

-- returns first position that as and bs do not match
moveTillNotMatch :: UArray Int Char -> UArray Int Char -> Int -> (Int -> Int) -> Int
moveTillNotMatch !as !bs !p f =
    if inRange (bounds as) p then
        if as ! p == bs ! p then moveTillNotMatch as bs (f p) f
        else p
    else p

-- returns first position that is not c or is out of range
moveTillNotSame :: UArray Int Char -> Int -> Char -> (Int -> Int) -> Int
moveTillNotSame !as !p !c f =
    if inRange (bounds as) p then
        if as ! p == c then moveTillNotSame as (f p) c f
        else p
    else p
