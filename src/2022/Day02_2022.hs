module Day02_2022 (
) where

data RPS = ROCK | PAPER | SCISSORS deriving Eq

data Winner = Player1 | Player2 | Draw deriving Eq

type Round = (RPS, RPS)

readPlayer1 :: Char -> RPS
readPlayer1 'A' = ROCK
readPlayer1 'B' = PAPER
readPlayer1 'C' = SCISSORS

readPlayer2 :: Char -> RPS
readPlayer2 'X' = ROCK
readPlayer2 'Y' = PAPER
readPlayer2 'Z' = SCISSORS

readRound :: [Char] -> Round
readRound cs = (readPlayer1 p1, readPlayer2 p2)
    where
        p1 = head cs
        p2 = cs !! 2

readData ::([Char] -> Round) -> IO [Round]
readData f = map f . lines <$> readFile "resource/2022/day02"

determineWinner :: RPS -> RPS -> Winner
determineWinner ROCK ROCK       = Draw
determineWinner ROCK PAPER      = Player2
determineWinner ROCK SCISSORS   = Player1
determineWinner PAPER ROCK       = Player1
determineWinner PAPER PAPER      = Draw
determineWinner PAPER SCISSORS   = Player2
determineWinner SCISSORS ROCK       = Player2
determineWinner SCISSORS PAPER      = Player1
determineWinner SCISSORS SCISSORS   = Draw

scoreWinner :: Winner -> Int
scoreWinner Player1 = 0
scoreWinner Draw = 3
scoreWinner Player2 = 6

scoreShape :: RPS -> Int
scoreShape ROCK = 1
scoreShape PAPER = 2
scoreShape SCISSORS = 3

scoreRound :: Round -> Int
scoreRound r = scoreWinner winner + scoreShape shape 
    where
        winner = uncurry determineWinner r
        shape = snd r

runPt1 = sum . map scoreRound <$> readData readRound

------------------
chooseOutcome :: RPS -> Char -> RPS
chooseOutcome a 'Y' = a --always a draw
chooseOutcome ROCK      'X' = SCISSORS
chooseOutcome ROCK      'Z' = PAPER
chooseOutcome PAPER     'X' = ROCK
chooseOutcome PAPER     'Z' = SCISSORS
chooseOutcome SCISSORS  'X' = PAPER
chooseOutcome SCISSORS  'Z' = ROCK

readRound' :: [Char] -> Round
readRound' cs = (p1, chooseOutcome p1 p2)
    where
        p1 = readPlayer1 $ head cs
        p2 = cs !! 2

runPt2 = sum . map scoreRound <$> readData readRound'