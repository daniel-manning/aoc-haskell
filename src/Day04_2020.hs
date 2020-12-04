module Day04_2020
    (
    ) where

    import Data.List (sort, find)
    import Data.String.Utils (split, splitWs, startswith, endswith)

    data Metric = CM | IN deriving Show
    data Height = Height Int Metric deriving Show
    data EyeColour = AMB | BLU | BRN | GRY | GRN | HZL | OTH deriving Show
    data Record = Record Int Int Int Height String String String (Maybe Int) deriving Show

    --constructRecord :: [String] -> Maybe Record
    readIntTag:: String -> [String] -> Maybe Int
    readIntTag tagName input = read . drop 4 <$> find (startswith tagName) input

    readBirthYear = readIntTag "byr:"
    readIssueYear = readIntTag "iyr:"
    readExpirationYear = readIntTag "eyr:"

    readHeight :: [String] -> Maybe Height
    readHeight input = (parseHeight . drop 4) =<< find (startswith "hgt:") input

    parseHeight :: String -> Maybe Height
    parseHeight = (\c -> if (endswith "cm" c || endswith "in" c) then Just (Height (read $ take ((length c) - 2) c) (toMetric $ take 2 $ drop ((length c) - 2) c)) else Nothing)

    toMetric :: String -> Metric
    toMetric "cm" = CM
    toMetric "in" = IN

    readEyeColour :: [String] -> Maybe EyeColour
    readEyeColour input = (toEyeColour . drop 4) =<< find (startswith "ecl:") input

    toEyeColour :: String -> Maybe EyeColour
    toEyeColour "amb" = Just AMB
    toEyeColour "blu" = Just BLU
    toEyeColour "brn" = Just BRN
    toEyeColour "gry" = Just GRY
    toEyeColour "grn" = Just GRN
    toEyeColour "hzl" = Just HZL
    toEyeColour "oth" = Just OTH
    toEyeColour "amb" = Nothing

    readPassportID :: [String] -> Maybe Int
    readPassportID input = (_ . drop 4) =<< find (startswith "ecl:") input

    runDocuments = map mergeTokeniseSort . groupBetweenBlankLines <$> readDocuments
    countValidDocuments = length . filter validate <$> runDocuments

    validate:: [String] -> Bool
    validate input = (8 == length input) || ((7 == length input) && (not $ any (startswith "cid:") input))

    mergeTokeniseSort :: [String] -> [String]
    mergeTokeniseSort input = sort $ splitWs $ foldr (\a b -> a ++ " " ++ b) "" input

    groupBetweenBlankLines :: [String] -> [[String]]
    groupBetweenBlankLines input = groupBetweenBlankLines'' input []

    groupBetweenBlankLines'' :: [String] -> [String] -> [[String]]
    groupBetweenBlankLines'' [] n = [n]
    groupBetweenBlankLines'' (x:xs) n | length x == 0 = n : groupBetweenBlankLines'' xs []
                                    | otherwise = groupBetweenBlankLines'' xs (x : n)

    readDocuments :: IO [String]
    readDocuments = lines <$> readFile "resource/2020/day04"