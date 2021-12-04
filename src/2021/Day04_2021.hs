module Day04_2021
    (
    ) where

    import Text.ParserCombinators.Parsec
    import Data.Either.Combinators
    import Data.List(find, transpose, delete)
    import Data.List.Extra(trim)
    import Data.Set(fromList, empty, union, Set)
    import Data.Maybe

    newtype Numbers = Numbers [Int] deriving Show
    newtype Card = Card [[Int]] deriving Show
    type C = [[Int]]
    data Status = Finished | Ongoing deriving (Eq, Show)
    -----------------
    parseBingoNumbers :: Parser Numbers
    parseBingoNumbers = do
        n <- sepBy1 (many1 digit) (char ',')
        return $ Numbers $ map read n

    parseBingoCardLine :: Parser [Int]
    parseBingoCardLine = do
        n <- sepBy1 (many1 digit) (many1 space)
        return $ map read n

    parseBingo :: (String, [[String]]) -> (Numbers, [Card])
    parseBingo (x, xs) = (fromRight' $ parse parseBingoNumbers "" x, map (Card . map (fromRight' . parse parseBingoCardLine "")) xs)

    readData :: IO [String]
    readData = map trim . lines <$> readFile "resource/2021/day04"

    splitOutNumbersCards :: [String] -> (String, [[String]]) 
    splitOutNumbersCards xs = (head xs, groupBetweenBlankLines (drop 2 xs))
        where
              groupBetweenBlankLines :: [String] -> [[String]]
              groupBetweenBlankLines input = map reverse $ groupBetweenBlankLines'' input []

              groupBetweenBlankLines'' :: [String] -> [String] -> [[String]]
              groupBetweenBlankLines'' [] n = [n]
              groupBetweenBlankLines'' (x:xs) n | null x = n : groupBetweenBlankLines'' xs []
                                                | otherwise = groupBetweenBlankLines'' xs (x : n)
    --------------------------

    {-- 
    If we expand the tables so they're a collection of rows AND columns then drawing a ball is removing every matching element in the card
    a win condition is just finding an empty array
    when one card wins we stop and shove everything left in a set (every number is unique in a card)
    count up whats left in the set and multiply by just called ball

    Test : 188 * 24 = 4512
    --}

    createFullLines :: [[a]] -> [[a]]
    createFullLines xs = xs ++ transpose xs

    reduceByDrawnBall :: Eq a => a -> [[a]] -> [[a]]
    reduceByDrawnBall n = map (delete n)

    checkForWin :: [[a]] -> Status
    checkForWin xs = if any null xs then Finished
                      else Ongoing

    takeRemaining :: Ord a => [[a]] -> Set a
    takeRemaining = foldl union empty . map fromList

    drawBallsUntilFinished :: (Numbers, [Card]) -> (C, Int)
    drawBallsUntilFinished (Numbers ns, cards) = drawBalls' ns (map (createFullLines . (\(Card c) -> c)) cards)
  
    drawBalls' (ball : balls) cs = case findWinner ball cs of
        Just x -> (x, ball)
        Nothing -> drawBalls' balls (reducedCard ball cs)
    
    reducedCard :: Int -> [C] -> [C]
    reducedCard n = map (reduceByDrawnBall n)

    findWinner :: Int -> [C] -> Maybe C
    findWinner n xs= find (\x -> checkForWin x == Finished) $ reducedCard n xs

    runPt1 :: (Numbers, [Card]) -> Int
    runPt1 = (\(x, n) -> n * (sum $ takeRemaining x)) . drawBallsUntilFinished
    ----------------------
    drawBallsUntilLastCard :: (Numbers, [Card]) -> (C, Int)
    drawBallsUntilLastCard (Numbers ns, cards) = drawBalls'' ns (map (createFullLines . (\(Card c) -> c)) cards)
  
    drawBalls'' (ball : balls) cs = if (null remaining) then (head (reducedCard ball cs), ball)
                                    else drawBalls'' balls remaining
        where
            remaining = filter (\x -> checkForWin x == Ongoing) (reducedCard ball cs)

    runPt2 :: (Numbers, [Card]) -> Int
    runPt2 = (\(x, n) -> n * (sum $ takeRemaining x)) . drawBallsUntilLastCard

    main = runPt2 . parseBingo . splitOutNumbersCards <$> readData