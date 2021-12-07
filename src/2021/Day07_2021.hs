module Day07_2021 
    (
    ) where


    import Text.ParserCombinators.Parsec (many1, digit, Parser, parse, char, sepBy1)
    import Data.Either.Combinators ( fromRight' )
    import Data.List(genericLength)
    import System.Posix.Internals (c_dup2)
    
    parseCrabs :: Parser [Int]
    parseCrabs = do
        n <- sepBy1 (many1 digit) (char ',')
        return $ map read n

    readData :: IO String
    readData = readFile "resource/2021/day07"

    readAndParse :: IO [Int]
    readAndParse = fromRight' . parse parseCrabs "" <$> readData
    ------------------
    mean :: (Fractional a1, Integral a2) => [a2] -> a1
    mean xs = fromIntegral (sum xs) / fromIntegral (genericLength xs)

    seriesMean :: IO Double
    seriesMean = mean <$> readAndParse

    g :: Num c => c -> [c] -> c
    g n = sum . map (abs . (\x -> x - n))

    pullWhileDecreasing :: Int -> Int -> (Int -> [Int] -> Int) -> [Int] -> Int
    pullWhileDecreasing old n f xs | k <= old = pullWhileDecreasing k (n-1) f xs
                                   | otherwise = old
        where k = f n xs

    runPt1 :: [Int] -> Int
    runPt1 xs = pullWhileDecreasing (g (maximum xs) xs) (maximum xs) g xs
    ------------------------
    g' :: Integral c => c -> [c] -> c
    g' n = sum . map (\x ->k x * kp x `div` 2)
        where
            k = abs . (\x -> x - n)
            kp x = 1 + k x

    runPt2 :: [Int] -> Int
    runPt2 xs = pullWhileDecreasing (g' (maximum xs) xs) (maximum xs) g' xs

    main :: IO Int
    main =  runPt2 <$> readAndParse