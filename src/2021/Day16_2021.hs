module Day16_2021 where


import qualified Data.Map.Strict as Map
import Data.Maybe ( fromJust ) 
import Data.String.Utils (rstrip)
import Data.List.Extra (chunksOf)
import Data.Foldable (concatMap)
import System.IO.HVFS (HVFSStat(vAccessTime))

data Bit = One | Zero deriving (Eq, Show)
data Packet = Packet Int Int [Packet] | Literal Int Int Int deriving Show
data LenType = Length Int | NoOfPacket Int

hexMap :: Map.Map Char [Bit]
hexMap = Map.fromList [
    ('0', [Zero, Zero, Zero, Zero]),
    ('1', [Zero, Zero, Zero, One]),
    ('2', [Zero, Zero, One, Zero]),
    ('3', [Zero, Zero, One, One]),
    ('4', [Zero, One, Zero, Zero]),
    ('5', [Zero, One, Zero, One]),
    ('6', [Zero, One, One, Zero]),
    ('7', [Zero, One, One, One]),
    ('8', [One, Zero, Zero, Zero]),
    ('9', [One, Zero, Zero, One]),
    ('A', [One, Zero, One, Zero]),
    ('B', [One, Zero, One, One]),
    ('C', [One, One, Zero, Zero]),
    ('D', [One, One, Zero, One]),
    ('E', [One, One, One, Zero]),
    ('F', [One, One, One, One])]

readData :: IO String
readData = rstrip <$> readFile "resource/2021/day16"
-------------------
readAndParse :: IO [Bit]
readAndParse = (\xs -> (\c -> fromJust $ Map.lookup c hexMap) =<< xs ) <$> readData

parseBits :: [Bit] -> (Packet, [Bit])
parseBits xs = parsePacket versionID typeID rest
    where
        versionID = bitsToInt $ take 3 xs
        typeID = bitsToInt $ take 3 $ drop 3 xs
        rest = drop 6 xs

takeWithFirstFailure:: (a -> Bool) -> [a] -> [a]
takeWithFirstFailure p [] = []
takeWithFirstFailure p (x:xs) | p x = x : takeWithFirstFailure p xs
                              | otherwise = [x]

parsePacket :: Int -> Int -> [Bit] -> (Packet, [Bit])
parsePacket v 4 r = (Literal v 4 csReal,  rest)
    where
        cs = takeWithFirstFailure (\xs -> head xs == One) $ chunksOf 5 r
        csReal = bitsToInt $ concatMap tail cs
        rest = drop (sum $ map length cs) r
parsePacket v t r = (Packet v t (fst ps), (snd ps))
    where
        lenType = head r
        len = if lenType == Zero then Length (bitsToInt $take 15 $ drop 1 r) else NoOfPacket (bitsToInt $ take 11 $ drop 1 r)
        ps = prepareSubPackets len r


prepareSubPackets :: LenType -> [Bit] -> ([Packet], [Bit])
prepareSubPackets _ [] = ([], [])
prepareSubPackets (Length n) r = (parseSubPackets (take n $ drop 16 r), drop (n + 16) r)
prepareSubPackets (NoOfPacket n) r = parseNTimes n (drop 12 r) [] 

parseSubPackets :: [Bit] -> [Packet]
parseSubPackets [] = []
parseSubPackets r = fst subpacket : parseSubPackets (snd subpacket)
    where
        subpacket = parseBits r

parseNTimes :: Int -> [Bit] -> [Packet] -> ([Packet], [Bit])
parseNTimes 0 bs ps = (reverse ps, bs)
parseNTimes n r ps = parseNTimes (n-1) (snd subpacket) (fst subpacket : ps)
    where
        subpacket = parseBits r

countUpVersionNumbers :: Packet -> Int
countUpVersionNumbers (Literal v _ _) = v
countUpVersionNumbers (Packet v _ ps) = v + sum (map countUpVersionNumbers ps)


bitsToInt :: [Bit] -> Int
bitsToInt xs =  sum $ zipWith (\k n -> if k == One then 2 ^ n else 0) (reverse xs) [0..] 

solution :: IO Int
solution = countUpVersionNumbers . fst . parseBits <$> readAndParse