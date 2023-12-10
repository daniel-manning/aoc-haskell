module Day07_2023 (
) where
import Data.Maybe (mapMaybe)
import Data.List (sort, group, isInfixOf)

data Card = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace   deriving (Eq, Ord, Show)
data HandType = HighCard | OnePair | TwoPair | ThreeOfAKind |  FullHouse | FourOfAKind | FiveOfAKind deriving (Eq, Ord, Show)
data HandAndBid = HandAndBid [Card] Int deriving (Eq, Show)

parseHandAndBid :: String -> HandAndBid
parseHandAndBid = (\l -> HandAndBid (parseHand (head l)) (read (l!!1))) . words

parseCard :: Char -> Maybe Card
parseCard 'A' = Just Ace
parseCard 'K' = Just King
parseCard 'Q' = Just Queen
parseCard 'J' = Just Jack
parseCard 'T' = Just Ten
parseCard '9' = Just Nine
parseCard '8' = Just Eight
parseCard '7' = Just Seven
parseCard '6' = Just Six
parseCard '5' = Just Five
parseCard '4' = Just Four
parseCard '3' = Just Three
parseCard '2' = Just Two
parseCard _ = Nothing

parseHand :: String -> [Card]
parseHand = mapMaybe parseCard
--------------------
readData :: IO [HandAndBid]
readData = map parseHandAndBid . lines <$> readFile "resource/2023/day07"

checkGroupSizes :: [Int] -> [Card] -> Bool
checkGroupSizes sizes cs = sizes == sort (map length $ group $ sort cs)

allTheSame :: [Card] -> Bool
allTheSame =  checkGroupSizes [5]

fourTheSame :: [Card] -> Bool
fourTheSame =  checkGroupSizes [1, 4]

fullHouse :: [Card] -> Bool
fullHouse = checkGroupSizes [2, 3]

threeOfAKind :: [Card] -> Bool
threeOfAKind = elem 3 . map length . group . sort

twoPair :: [Card] -> Bool
twoPair = isInfixOf [2,2] . sort . map length . group . sort

onePair :: [Card] -> Bool
onePair = elem 2 . map length . group . sort

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

runPt1 :: IO Integer
runPt1 = scoreHandAndBids . sort <$> readData