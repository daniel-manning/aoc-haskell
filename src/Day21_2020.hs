module Day21_2020
    (
    ) where

    import Text.ParserCombinators.Parsec
    import Data.Either.Combinators
    import Data.List (nub, foldl1', sortBy, (\\))
    import Data.Set (Set)
    import qualified Data.Set as Set

    data Food = Food [String] [String] deriving Show

    parseWord :: Parser String
    parseWord = do
        spaces
        w <- many1 letter
        spaces
        return w

    parseFood :: Parser Food
    parseFood = do
        ingredients <- many1 parseWord
        string "(contains"
        allergies <- sepBy1 parseWord (char ',')
        char ')'
        return $ Food ingredients allergies

    allAllergies :: [Food] -> [String]
    allAllergies fs = nub $ (\(Food _ as) -> as) =<< fs

    findPotentialAllergyIngredients :: [Food] -> String -> [String]
    findPotentialAllergyIngredients list allergy = Set.toList $ foldl1' Set.intersection $ map (\(Food l _) -> Set.fromList l) $ filter (\(Food _ al) -> allergy `elem` al) list

    sortCandidateList = sortBy (\(xs, _) (ys, _) -> compare (length xs) (length ys))

    reduceList :: [([String], String)] -> [(String, String)]
    reduceList [] = []
    reduceList ((x, a):xs) = (head x,a): reduceList (sortCandidateList $ map (\(l,a) -> ( l \\ x, a)) xs)

    findAllergyIngredients :: [Food] -> [(String, String)]
    findAllergyIngredients = reduceList . sortCandidateList . (\l -> map (\a -> (findPotentialAllergyIngredients l a, a)) $ allAllergies l)

    findFoodLists :: IO [Food]
    findFoodLists = map (fromRight' . parse parseFood "") <$> inputAllergyList

    allIngredientLists = map (\(Food xs _) -> xs)

    day21Pt1 = (\fs -> length $ (\l -> l \\ map fst (findAllergyIngredients fs)) =<< allIngredientLists fs) <$> findFoodLists

    day21Pt2 = foldl1' (\a b -> a ++ "," ++ b) . map fst . sortBy (\(iA,aA) (iB, aB) -> compare aA aB) . findAllergyIngredients <$> findFoodLists
    
    inputAllergyList :: IO [String]
    inputAllergyList = lines <$> readFile "resource/2020/day21"
        