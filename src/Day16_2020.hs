module Day16_2020
    (
    ) where

    import Text.ParserCombinators.Parsec
    import Data.Either.Combinators

    data Range = Range Int Int deriving Show

    data Field = Field String Range Range deriving Show
    newtype Ticket = Ticket [Int]

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

    checkTicketsForInvalidData :: [Field] -> Ticket -> [Int]
    checkTicketsForInvalidData f (Ticket ts) =  filter (\n -> not $ any (`dataInRange` n) f) ts
     
    dataInRange :: Field -> Int -> Bool
    dataInRange (Field _ (Range x y) (Range l m)) n = (n >= x && n <= y) || (n >= l && n <= m)

    ticketData :: IO ([Field], Ticket, [Ticket])
    ticketData = (\x -> (fieldList $ head x, ticket (x !! 1), parseTickets $ tail (x !! 2))) . groupBetweenBlankLines <$> readTicketData

    groupBetweenBlankLines :: [String] -> [[String]]
    groupBetweenBlankLines input = map reverse $ groupBetweenBlankLines'' input []

    groupBetweenBlankLines'' :: [String] -> [String] -> [[String]]
    groupBetweenBlankLines'' [] n = [n]
    groupBetweenBlankLines'' (x:xs) n | length x == 0 = n : groupBetweenBlankLines'' xs []
                                      | otherwise = groupBetweenBlankLines'' xs (x : n)


    findInvalidData :: ([Field], Ticket, [Ticket]) -> [Int]
    findInvalidData (f, t, ts) = (checkTicketsForInvalidData f) =<< ts

    day16Pt1 = sum . findInvalidData <$> ticketData

    readTicketData :: IO [String]
    readTicketData = lines <$> readFile "resource/2020/day16"                                  
