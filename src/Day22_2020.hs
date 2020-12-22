module Day22_2020
    (
    ) where

    import Text.ParserCombinators.Parsec
    import Data.Either.Combinators
    import ListUtils

    newtype Deck = Deck [Int] deriving Show
    data Player = Player String Deck deriving Show

    parseCard :: Parser Int
    parseCard = do 
        n <- many1 digit
        return $ read n

    parseID :: Parser String
    parseID = do
        string "Player "
        n <- many1 digit
        char ':'
        return n


    playCombat :: Player -> Player -> Player
    playCombat p (Player _ (Deck [])) = p
    playCombat (Player _ (Deck [])) p = p
    playCombat (Player a (Deck (da:das))) (Player b (Deck (db:dbs))) | da > db = playCombat (Player a (Deck (das ++ [da, db]))) (Player b (Deck dbs))
                                                                     | otherwise = playCombat (Player a (Deck das)) (Player b (Deck (dbs ++ [db, da])))

    scoreWinningDeck (Deck xs) = sum $ zipWith (*) (reverse xs) [1..]
    
    day22Pt1 = scoreWinningDeck . (\(Player _ d) -> d) . (\x -> playCombat (head x) (x!!1)) <$> makePlayersDecks

    makePlayersDecks :: IO [Player]
    makePlayersDecks =  map (\x -> Player ((fromRight' . parse parseID "") (head x)) (Deck $ map (fromRight' . parse parseCard "") (tail x))) . groupBetweenBlankLines <$> inputDecks


    inputDecks :: IO [String]
    inputDecks = reverse . lines <$> readFile "resource/2020/day22"