module Day07_2023_pt2 (
) where
import Data.Maybe (mapMaybe, fromMaybe)
import Data.List (sort, group, isInfixOf, find)

data Card = Joker | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Queen | King | Ace   deriving (Eq, Ord, Show)
data HandType = HighCard | OnePair | TwoPair | ThreeOfAKind |  FullHouse | FourOfAKind | FiveOfAKind deriving (Eq, Ord, Show)
data HandAndBid = HandAndBid [Card] Int deriving (Eq, Show)
data HandAndHandType = HandAndHandType [Card] HandType deriving (Eq, Show)

parseHandAndBid :: String -> HandAndBid
parseHandAndBid = (\l -> HandAndBid (parseHand (head l)) (read (l!!1))) . words

parseCard :: Char -> Maybe Card
parseCard 'A' = Just Ace
parseCard 'K' = Just King
parseCard 'Q' = Just Queen
parseCard 'T' = Just Ten
parseCard '9' = Just Nine
parseCard '8' = Just Eight
parseCard '7' = Just Seven
parseCard '6' = Just Six
parseCard '5' = Just Five
parseCard '4' = Just Four
parseCard '3' = Just Three
parseCard '2' = Just Two
parseCard 'J' = Just Joker
parseCard _ = Nothing

parseHand :: String -> [Card]
parseHand = mapMaybe parseCard
--------------------
readData :: IO [HandAndBid]
readData = map parseHandAndBid . lines <$> readFile "resource/2023/day07"

groupSizes :: [Card] -> [(Int, Card)]
groupSizes = map (\l -> (length l, head l)) . group . sort

hasNWithoutJokers :: Int -> [(Int, Card)] -> Bool
hasNWithoutJokers n = any (\(m, c) -> n == m && c /= Joker)

noOfJokers :: [(Int, Card)] -> Int
noOfJokers = maybe 0 fst . find (\(_, c) -> c == Joker)

hasNWithJokers :: Int -> [(Int, Card)] -> Bool
hasNWithJokers n ics = any (\(m, c) -> (m + jokers) >= n) icsWithoutJokers
    where
        icsWithoutJokers = filter (\(_, c) -> c /= Joker) ics
        jokers = noOfJokers ics 

canMakeN :: Int ->  [Card] -> Bool
canMakeN n c = hasNWithoutJokers n ics || hasNWithJokers n ics || jokers == n
    where
        ics = groupSizes c
        jokers = noOfJokers ics

allTheSame :: [Card] -> Bool
allTheSame = canMakeN 5

fourTheSame :: [Card] -> Bool
fourTheSame =  canMakeN 4

threeOfAKind :: [Card] -> Bool
threeOfAKind = canMakeN 3

onePair :: [Card] -> Bool
onePair = canMakeN 2

fullHouse :: [Card] -> Bool
fullHouse c = hasFullHouse || (hasThree && joker >= 1) || (hasTwoPair && joker >= 1) || (hasPair && joker >= 2)
    where 
        ics = groupSizes c
        icsWithoutJokers = filter (\(_, c') -> c' /= Joker) ics
        hasFullHouse = isInfixOf [2,3] $ sort $ map fst icsWithoutJokers
        hasThree = any ((>=3) . fst) icsWithoutJokers
        hasTwoPair = isInfixOf [2,2] $ sort $ map fst icsWithoutJokers
        hasPair = any ((>=2) . fst) icsWithoutJokers
        joker = noOfJokers ics

twoPair :: [Card] -> Bool
twoPair c = hasTwoPairs || (hasPair && joker >= 1) || (joker >= 2)
    where 
        ics = groupSizes c
        icsWithoutJokers = filter (\(_, c') -> c' /= Joker) ics
        hasTwoPairs = isInfixOf [2,2] $ sort $ map fst icsWithoutJokers
        hasPair = any ((>= 2) . fst) icsWithoutJokers
        joker = noOfJokers ics

quantifyHand :: HandAndBid -> HandType
quantifyHand (HandAndBid hand _) | allTheSame hand = FiveOfAKind
                                 | fourTheSame hand = FourOfAKind
                                 | fullHouse hand = FullHouse
                                 | threeOfAKind hand = ThreeOfAKind
                                 | twoPair hand = TwoPair
                                 | onePair hand = OnePair
                                 | otherwise = HighCard

instance Ord HandAndBid where
    compare hb1 hb2 | h1 == h2 = tiebreakEqualHandType hb1 hb2
                    | otherwise = compare h1 h2
        where
           h1 = quantifyHand hb1
           h2 = quantifyHand hb2

tiebreakEqualHandType :: HandAndBid -> HandAndBid -> Ordering
tiebreakEqualHandType (HandAndBid h1 _) (HandAndBid h2 _) = tiebreakEqualHandType' h1 h2
    where
        tiebreakEqualHandType' h1 h2 | head h1 == head h2 = tiebreakEqualHandType' (tail h1) (tail h2)
                                     | otherwise = compare h1 h2

scoreHandAndBids :: [HandAndBid] -> Integer
scoreHandAndBids = sum . zipWith (curry (\ (n, HandAndBid _ bid) -> n * toInteger bid)) [1..]

toHandAndHandType :: HandAndBid -> HandAndHandType
toHandAndHandType hb@(HandAndBid hand _) = HandAndHandType hand (quantifyHand hb)

debug :: IO [HandAndHandType]
debug = map toHandAndHandType . sort <$> readData

runPt2 :: IO Integer
runPt2 = scoreHandAndBids . sort <$> readData
