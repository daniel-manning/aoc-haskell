module Day04_2020
    (
    ) where

    import Data.List (sort, find)
    import Data.Char (isDigit)
    import Data.Maybe (isJust)
    import Data.String.Utils (split, splitWs, startswith, endswith)

    data Metric = CM | IN deriving Show
    data Height = Height Int Metric deriving Show
    data EyeColour = AMB | BLU | BRN | GRY | GRN | HZL | OTH deriving Show
    data Record = Record Int Int Int Height String EyeColour String (Maybe String) deriving Show

    countValidDataRecords:: IO Int
    countValidDataRecords = (length . filter isJust <$> map validateData) <$> runDocuments

    validateData :: [String] -> Maybe Record
    validateData input = pure Record
                        <*> readBirthYear input
                        <*> readIssueYear input
                        <*> readExpirationYear input
                        <*> readHeight input
                        <*> readHairColour input
                        <*> readEyeColour input
                        <*> readPassportID input
                        <*> readCountryID input

    --constructRecord :: [String] -> Maybe Record
    readIntTag:: String -> Int -> Int -> [String] -> Maybe Int
    readIntTag tagName least most input = (\n -> if(n >= least && n <= most) then Just n else Nothing) =<< (read . drop 4 <$> find (startswith tagName) input)

    readBirthYear = readIntTag "byr:" 1920 2002
    readIssueYear = readIntTag "iyr:" 2010 2020
    readExpirationYear = readIntTag "eyr:" 2020 2030

    readHeight :: [String] -> Maybe Height
    readHeight input = validateHeight =<< (parseHeight . drop 4) =<< find (startswith "hgt:") input

    parseHeight :: String -> Maybe Height
    parseHeight = (\c -> if (endswith "cm" c || endswith "in" c) then Just (Height (read $ take ((length c) - 2) c) (toMetric $ take 2 $ drop ((length c) - 2) c)) else Nothing)

    validateHeight :: Height -> Maybe Height
    validateHeight (Height h CM) = if(h >= 150 && h <= 193) then Just (Height h CM) else Nothing
    validateHeight (Height h IN) = if(h >= 59  && h <= 76) then Just (Height h IN) else Nothing

    toMetric :: String -> Metric
    toMetric "cm" = CM
    toMetric "in" = IN

    readHairColour :: [String] -> Maybe String
    readHairColour input = (validateHairColour . drop 4) =<< find (startswith "hcl:") input

    validateHairColour :: String -> Maybe String
    validateHairColour c = if(startswith "#" c && all (\l ->  l `elem` "0123456789abcdef") (drop 1 c)) then Just c else Nothing

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
    toEyeColour _     = Nothing

    readPassportID :: [String] -> Maybe String
    readPassportID input = (validatePassportID . drop 4) =<< find (startswith "pid:") input

    validatePassportID :: String -> Maybe String
    validatePassportID input = if(9 == (length input) && all isDigit input) then Just input else Nothing

    readCountryID :: [String] -> Maybe (Maybe String)
    readCountryID input = Just (drop 4 <$> find (startswith "cid:") input)

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