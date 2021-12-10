module Day10_2021 where
import Data.List (sort)
import Data.Either.Combinators (isLeft, fromLeft')

readData :: IO [String]
readData = lines <$> readFile "resource/2021/day10"

---------------

isNewOpener :: Char -> Bool
isNewOpener '(' = True 
isNewOpener '[' = True 
isNewOpener '{' = True 
isNewOpener '<' = True
isNewOpener _   = False

matchingCloser :: Char -> Char -> Bool
matchingCloser '(' p = p == ')'
matchingCloser '[' p = p == ']'
matchingCloser '{' p = p == '}'
matchingCloser '<' p = p == '>'
matchingCloser _  _= False

runCorruptedLines :: [Char] -> [Char] -> Either Char Bool
runCorruptedLines [] [] = Right True
runCorruptedLines (p:ps) [] = Right False 
runCorruptedLines [] (p: ps) = runCorruptedLines [p] ps
runCorruptedLines (c:cs) (p:ps) | isNewOpener p = runCorruptedLines (p : c : cs) ps
                                | matchingCloser c p = runCorruptedLines cs ps
                                | otherwise = Left p

scoreSyntaxChecker :: Char -> Int
scoreSyntaxChecker ')' = 3
scoreSyntaxChecker ']' = 57
scoreSyntaxChecker '}' = 1197
scoreSyntaxChecker '>' = 25137
scoreSyntaxChecker _ = 0

illegalChars :: [[Char]] -> Int
illegalChars = sum . map (scoreSyntaxChecker. fromLeft') .  filter isLeft . map (runCorruptedLines [])

runPt1 :: [[Char]] -> Int
runPt1 = illegalChars
--------------------------

runIncompleteLines :: [Char] -> [Char] -> Either String Bool
runIncompleteLines [] [] = Right True
runIncompleteLines ps [] = Left (map makeCloser ps) 
runIncompleteLines [] (p: ps) = runIncompleteLines [p] ps
runIncompleteLines (c:cs) (p:ps) | isNewOpener p = runIncompleteLines (p : c : cs) ps
                                 | matchingCloser c p = runIncompleteLines cs ps
                                 | otherwise = Right False

makeCloser :: Char -> Char
makeCloser '(' = ')'
makeCloser '[' = ']'
makeCloser '{' = '}'
makeCloser '<' = '>'
makeCloser _ = '@'  {- I Know -}

incompleteChunks = map (runIncompleteLines [])

letterScore :: Char -> Int
letterScore ')' = 1
letterScore ']' = 2
letterScore '}' = 3
letterScore '>' = 4
letterScore  _  = 0 {- I Know -}

scoreAutoComplete :: [Char] -> Int
scoreAutoComplete = foldl (\b a -> b*5 + letterScore a) 0

takeMiddleScore :: Ord a => [a] -> a
takeMiddleScore xs = sort xs !! k
    where
        k = (length xs - 1) `div` 2 

runPt2 :: [[Char]] -> Int
runPt2 = takeMiddleScore . map (scoreAutoComplete . fromLeft') .  filter isLeft . incompleteChunks

solution :: IO Int
solution =  runPt2 <$> readData