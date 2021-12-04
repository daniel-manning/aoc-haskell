module Day12_2015
    (
    ) where

    import Text.ParserCombinators.Parsec
    import Data.Either.Combinators
    import Data.List(find)
    import Data.Maybe

    data Edge = E String String Int deriving Show
    data UDEdge = UDE String String Int Int deriving Show
        
    parseSentence =  do
            from <- many1 letter
            string " would "
            gainOrLose <- string "gain" <|> string "lose"
            space
            value <- many1 digit
            string " happiness units by sitting next to "
            to <- many1 letter
            char '.'
            return $ E from to (correct gainOrLose (read value))

    correct "gain" x = x
    correct "lose" x = (-1)*x
    --------------------

    --Task for PT1 find a hamiltonian cycle in edge network
    bidirectional :: [Edge] -> [UDEdge] -> [UDEdge]
    bidirectional [] ud = ud
    bidirectional ((E t f c):xs) ud | any (\(UDE ut uf uct ucf) -> (ut == t && uf == f)|| (uf == t && ut == f)) ud = bidirectional xs ud
                                    | otherwise = bidirectional xs (UDE t f c (ucf xs): ud)
        where
            ucf xs = (\(E _ _ cf) -> cf) $ fromJust $ find (\(E a b d) -> a == f && b == t) xs

    test = filter (\(UDE _ _ ct cf) -> ct + cf >= 0)

    --main:: IO [Edge]
    main =  map (fromRight' . parse parseSentence "") . lines <$> readFile "resource/2015/day13"