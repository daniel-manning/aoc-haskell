module Day22_2020
    (
        day22Pt2
    ) where

    import Text.ParserCombinators.Parsec
    import Data.Either.Combinators
    import Data.Maybe (fromJust)
    import Data.List (find)
    import ListUtils

    newtype Deck = Deck [Int] deriving (Eq, Show)
    data Player = Player String Deck deriving (Eq, Show)

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


    playRecursiveCombat :: [(Player, Player)] -> Player -> Player -> Player
    playRecursiveCombat _ p (Player _ (Deck [])) = p
    playRecursiveCombat _ (Player _ (Deck [])) p = p
    playRecursiveCombat ps pa@(Player a (Deck (da:das))) pb@(Player b (Deck (db:dbs))) | (pa, pb) `elem` ps = pa
                                                                                       | da <= length das && db <= length dbs = subgame subgameWinner
                                                                                       | da > db = playRecursiveCombat ((pa,pb):ps) (Player a (Deck (das ++ [da, db]))) (Player b (Deck dbs))
                                                                                       | otherwise = playRecursiveCombat ((pa,pb):ps) (Player a (Deck das)) (Player b (Deck (dbs ++ [db, da])))                                            
        where
            subgame (Player c d) = if(c == a) then playRecursiveCombat ((pa,pb):ps) (Player a (Deck (das ++ [da, db]))) (Player b (Deck dbs)) else playRecursiveCombat ((pa,pb):ps) (Player a (Deck das)) (Player b (Deck (dbs ++ [db, da])))
            subgameWinner = playRecursiveCombat [] (Player a (Deck (take da das))) (Player b (Deck (take db dbs)))

    scoreWinningDeck (Deck xs) = sum $ zipWith (*) (reverse xs) [1..]
    
    day22Pt1 = scoreWinningDeck . (\(Player _ d) -> d) . (\x -> playCombat (head x) (x!!1)) <$> makePlayersDecks

    day22Pt2 = scoreWinningDeck . (\(Player _ d) -> d) . (\x -> playRecursiveCombat [] (fromJust $ find (\(Player id _) -> id == "1") x) (fromJust $ find (\(Player id _) -> id == "2") x)) <$> makePlayersDecks

    makePlayersDecks :: IO [Player]
    makePlayersDecks =  map (\x -> Player ((fromRight' . parse parseID "") (head x)) (Deck $ map (fromRight' . parse parseCard "") (tail x))) . groupBetweenBlankLines <$> inputDecks


    inputDecks :: IO [String]
    inputDecks = reverse . lines <$> readFile "resource/2020/day22"


    {-- PT2
    32665

    real	9m37.973s
    user	9m40.291s
    sys	0m1.617s
    --}