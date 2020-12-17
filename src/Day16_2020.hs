module Day16_2020
    (
    ) where

    import Text.ParserCombinators.Parsec
    import Data.Either.Combinators (fromRight')
    import Data.List (transpose, delete)
    import Data.Functor (($>))
    import Data.List.Utils (startswith)
    import Data.Either
    import Data.Bifunctor

    data Range = Range Integer Integer deriving (Eq, Show)

    data Field = Field String Range Range deriving (Eq, Show)
    newtype Ticket = Ticket [Integer] deriving Show

    parseRange :: Parser Range
    parseRange = do
      m <- many1 digit
      char '-'
      n <- many1 digit
      return $ Range (read m) (read n)

    parseField :: Parser Field
    parseField = do
        name <- many1 (choice [try letter, try space])
        string ": "
        rangeA <- parseRange
        string " or "
        rangeB <- parseRange
        return $ Field name rangeA rangeB

    parseTicket :: Parser Ticket
    parseTicket = do
        n <- sepBy1 (many1 digit) (char ',')
        return $ Ticket $ map read n
    

    --test input = parse parseField "" input

    fieldList = map (fromRight' . parse parseField "")
    ticket = fromRight' . parse parseTicket "" . head . tail
    parseTickets = map (fromRight' . parse parseTicket "" )

    checkTicketsForInvalidData :: [Field] -> Ticket -> [Integer]
    checkTicketsForInvalidData f (Ticket ts) =  filter (\n -> not $ any (`dataInRange` n) f) ts
     
    dataInRange :: Field -> Integer -> Bool
    dataInRange (Field _ (Range x y) (Range l m)) n = (n >= x && n <= y) || (n >= l && n <= m)

    ticketData :: IO ([Field], Ticket, [Ticket])
    ticketData = (\x -> (fieldList $ head x, ticket (x !! 1), parseTickets $ tail (x !! 2))) . groupBetweenBlankLines <$> readTicketData

    groupBetweenBlankLines :: [String] -> [[String]]
    groupBetweenBlankLines input = map reverse $ groupBetweenBlankLines'' input []

    groupBetweenBlankLines'' :: [String] -> [String] -> [[String]]
    groupBetweenBlankLines'' [] n = [n]
    groupBetweenBlankLines'' (x:xs) n | length x == 0 = n : groupBetweenBlankLines'' xs []
                                      | otherwise = groupBetweenBlankLines'' xs (x : n)


    findInvalidData :: ([Field], Ticket, [Ticket]) -> [Integer]
    findInvalidData (f, t, ts) = (checkTicketsForInvalidData f) =<< ts

    day16Pt1 = sum . findInvalidData <$> ticketData

-----

    filterInvalidData :: ([Field], Ticket, [Ticket]) -> ([Field], Ticket, [Ticket])
    filterInvalidData (f, t, ts) = (f, t, filter (\t -> (length $ checkTicketsForInvalidData f t) == 0) ts)


    groupedFields :: [Ticket] -> [[Integer]]
    groupedFields ts =  transpose $ map (\(Ticket xs) -> xs) ts

    groupFieldData (f, t, ts) = (f, t, groupedFields ts)

    findFieldsMatchingData :: [Field] -> [Integer] -> [Field]
    findFieldsMatchingData fs is = filter (\f -> all (\n -> dataInRange f n) is) fs

    whileLeft :: ([Either a b] -> [Either a b]) -> [Either a b] -> [Either a b]
    whileLeft f a | any isLeft a = whileLeft f $! f a
                  | otherwise    = a

    reduceOptions :: (Eq a) => [[a]] -> [a]
    reduceOptions as = map fromRight' $! whileLeft (reduceOptions'' []) (map Left as)

    removeFromLeftList :: (Eq a) => a -> [Either [a] a] -> [Either [a] a]
    removeFromLeftList a xs = map (bimap (\l -> delete a l) id) xs

    reduceOptions'' :: (Eq a) => [Either [a] a] -> [Either [a] a] -> [Either [a] a]
    reduceOptions'' xs [] = xs
    reduceOptions'' xs ((Right a): as) = reduceOptions'' (xs ++ [(Right a)]) as
    reduceOptions'' xs ((Left a): as) | (length a) == 1 =  reduceOptions'' ((removeFromLeftList (head a) xs) ++ [(Right (head a))]) (removeFromLeftList (head a) as)
                                      | otherwise       = reduceOptions'' (xs ++ [(Left a)]) as

    findMatchingColumn fs is = reduceOptions $ map (findFieldsMatchingData fs) is

    matchTicketToColumn :: [Field] -> Ticket -> [(Integer, String)]
    matchTicketToColumn fs (Ticket ts) = zip ts $ map (\(Field name _ _) -> name) fs

    day16Pt2 = product . map fst . (\(f,t,is) -> filter (\(n, name) -> startswith "departure" name) $ matchTicketToColumn (findMatchingColumn f is) t) . groupFieldData . filterInvalidData <$> ticketData
    
    readTicketData :: IO [String]
    readTicketData = lines <$> readFile "resource/2020/day16"                                  
