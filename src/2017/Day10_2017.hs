module Day10_2017 (
) where

import qualified Data.List.PointedList.Circular as PL
import Data.Maybe (fromJust)
import Data.List (foldl')

type Code = PL.PointedList Int

test :: Code
test = fromJust $ PL.fromList [0, 1, 2, 3, 4]

split :: Char -> String -> [String]
split c s = reverse $ split' c s [] []
    where
        split' :: Char -> String -> String -> [String] -> [String]
        split' c [] coll cs = reverse coll : cs
        split' c (s:ss) coll cs | s == c = split' c ss [] (reverse coll: cs)
                                | otherwise = split' c ss (s : coll) cs

readData :: IO [Int]
readData =  map read . split ',' <$> readFile "resource/2017/day10"
---------------------------

knot :: Code -> Int -> Int -> Code
knot c l s = c'''
    where 
        (as, c') = takeN c l
        c'' :: Code
        c'' = maybe (fromJust $ PL.fromList as) (\l -> insert l (reverse as)) c'
        c''' = PL.moveN (l + s) c''

takeN :: PL.PointedList a -> Int -> ([a], Maybe (PL.PointedList a))
takeN pl n = takeN' pl n []
    where
        takeN':: PL.PointedList a -> Int -> [a] -> ([a], Maybe (PL.PointedList a))
        takeN' pl 0 xs = (xs, Just pl)
        takeN' pl n xs | PL.length pl == 1 && n == 1 = (PL._focus pl : xs, Nothing)
                       | otherwise = takeN' (fromJust $ PL.delete pl) (n-1) (PL._focus pl : xs)

insert :: PL.PointedList a -> [a] -> PL.PointedList a
insert = foldl' (flip PL.insertLeft)

applyLengths :: Code -> [Int] -> Code
applyLengths c = snd . foldl' (\b n -> (fst b + 1, knot (snd b) n (fst b))) (0, c)

--trial = 4
trial = 255

cursorPosition ns = (sum ns + (length ns)*(length ns - 1) `div` 2) `mod` (trial + 1)

findFirstTwoPositions :: [Int] -> Code -> (Int, Int)
findFirstTwoPositions ns c = (\pl -> (PL._focus $ PL.moveN (-1) pl, PL._focus pl)) $ PL.moveN (1-cPos) c
    where
        cPos = cursorPosition ns

runPt1 :: IO Int
runPt1 = (\ns ->  uncurry (*) $ findFirstTwoPositions ns $ applyLengths (fromJust $ PL.fromList [0..trial]) ns) <$> readData