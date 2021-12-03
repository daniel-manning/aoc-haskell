module Day13_2020
    (
    ) where

    import Text.ParserCombinators.Parsec
    import Text.ParserCombinators.Parsec.Token
    import Data.Either.Combinators
    import Data.Bifunctor (second) 
    import Data.List (minimumBy)

    data Bus = BusId Integer | OutofService deriving (Eq, Show)

    parseOutOfService = do
        char 'x'
        return OutofService

    parseBusId = do
        n <- many1 digit
        return $ BusId (read n)
    
    parseSchedule :: Parser [Bus]
    parseSchedule = sepBy (choice [try parseOutOfService, try parseBusId]) (char ',') 


    findNextBus (time, buses) = minimumBy (\a b -> compare (snd a) (snd b)) $ map (\(BusId n) -> (BusId n, n - (time `mod` n))) buses

    idByMinuites (BusId n, wait) = n * wait

    day13Pt1 = idByMinuites . findNextBus . (\x -> (read (head x), filter (/= OutofService) . fromRight' $ parse parseSchedule "" (x !! 1))) <$> readDetails

    {-- I don't know what's wrong with this
        --euclid :: Int -> Int -> ()
    euclid a b | b < a = euclid b a
               | otherwise = (\((q1, r1),(q2, r2),(s1, t1),(s2, t2)) -> (s2, t2, r2)) $ last $ extendedEuclid (0, a) (0, b) (1, 0) (0, 1)

    --extendedEuclid :: Int -> Int -> (Int, Int, Int)
    extendedEuclid (q1, 0) (q2, _) (s1, t1) (s2, t2) = []
    extendedEuclid (q1, _) (q2, 0) (s1, t1) (s2, t2) = []
    extendedEuclid (q1, r1) (q2, r2) (s1, t1) (s2, t2) =
        ((q1, r1),(q2, r2),(s1, t1),(s2, t2)) : extendedEuclid (q3, r3) (q4, r4) (s3, t3) (s2 - q4*s3, t2 - q4 * t3)
           where 
               q3 = r1 `div` r2
               r3 = r1 `mod` r2
               q4 = r2 `div` r3
               r4 = r2 `mod` r3
               s3 = s1 - q3 * s2
               t3 = t1 - q3 * t2 --}

    eGCD :: Integer -> Integer -> (Integer, Integer, Integer) 
    eGCD 0 b = (b, 0, 1)
    eGCD a b = let (g, s, t) = eGCD (b `mod` a) a
       in (g, t - (b `div` a) * s, s)


    chineseRemainderTheorem list = (sum yList `mod` m, m)
        where
            m = product $ map snd list
            mList = map ((\n -> m `div` n) . snd) list
            yList = zipWith (curry ((\ (a, b, c, (d, y, f)) -> c * a * (y `mod` b)) . (\ (a, (c, b)) -> (a, b, c, eGCD a b)))) mList list


    
    day13Pt2 = (\x -> fst . chineseRemainderTheorem . map (\(a, BusId n) -> (-a, n)) . filter (\(_, s) -> s /=OutofService) $ zip [0..] . fromRight' $ parse parseSchedule "" (x !! 1) )<$> readDetails


    readDetails :: IO [String]
    readDetails = lines <$> readFile "resource/2020/day13"