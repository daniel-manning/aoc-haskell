module Day14_2019
    (
    ) where

import Text.ParserCombinators.Parsec
    ( digit, letter, string, many1, sepBy1, parse, Parser )
import Data.Either.Combinators (fromRight')
import Data.Set (Set)
import qualified Data.Set as Set(delete, filter, fromList, toList, insert, size)
import Data.Maybe (listToMaybe, fromMaybe)
import Debug.Trace (trace)

data Ingredient = Ingredient String Int deriving (Eq, Ord, Show)
data Recipie = Recipie [Ingredient] Ingredient deriving (Eq, Ord, Show)
type RecipieBook = [Recipie]
---------------
parseIngredient =  do
        n <- many1 digit
        string " "
        name <- many1 letter
        return $ Ingredient name (read n)

parseIngredients :: Parser [Ingredient]
parseIngredients = sepBy1 parseIngredient (string ", ")

parseRecipie = do
        requirements <- parseIngredients
        string " => "
        Recipie requirements <$> parseIngredient
------------------
readData = map (fromRight' . parse parseRecipie "") . lines <$> readFile "resource/2019/day14"

substitute :: Ingredient -> Set Ingredient -> RecipieBook -> Set Ingredient
substitute i@(Ingredient name quantity) ingredients recipies = ingredients'
    where
        replacements = updateQuantities quantity . (\(Recipie xs (Ingredient name' q)) -> (q, xs)) . head $ filter (\(Recipie _ (Ingredient name' _)) -> name' == name) recipies --need to handle multipliers
        ingredients' = foldl updateIngredients (Set.delete i ingredients) replacements

updateQuantities :: Int -> (Int, [Ingredient]) -> [Ingredient]
updateQuantities neededQuantity (recipieQuantity, xs) = map (\(Ingredient n q) -> Ingredient n (multiplier * q)) xs
    where
        multiplier = ceiling ( fromIntegral neededQuantity / fromIntegral recipieQuantity)

updateIngredients :: Set Ingredient -> Ingredient -> Set Ingredient
updateIngredients si (Ingredient name quantity) = si'
    where
       m = listToMaybe $ Set.toList $ Set.filter (\(Ingredient n _) -> n == name) si
       updatedIngredient = maybe (Ingredient name quantity) (\i -> Ingredient name (quantity + (\(Ingredient _ q) -> q) i)) m
       si' = Set.insert updatedIngredient $ maybe si (`Set.delete` si) m

directBuildableComponents :: RecipieBook -> [String]
directBuildableComponents = map (\(Recipie _ (Ingredient name _)) -> name) . filter (\(Recipie xs _) -> any (\(Ingredient name _) -> name == "ORE") xs )


--dont convert to ore as you go because there will be savings in the conversion
findComponents :: Set Ingredient -> RecipieBook -> Set Ingredient
findComponents si rb | (Set.size si == length dc) && all (\(Ingredient n _) -> n `elem` ("ORE":dc)) (Set.toList si) = findDirectComponents si rb
                     | otherwise = trace (show si) findComponents (substitute (head ings) si rb) rb
    where
        dc = directBuildableComponents rb
        ings = filter (\(Ingredient n _) -> n `notElem` ("ORE": dc)) $ Set.toList si

findDirectComponents :: Set Ingredient -> RecipieBook -> Set Ingredient
findDirectComponents si rb | (Set.size si == 1) && any (\(Ingredient n _) -> n == "ORE") (Set.toList si) = si
                     | otherwise = trace (show si) findDirectComponents (substitute (head ings) si rb) rb
    where
        ings = filter (\(Ingredient n _) -> n /= "ORE") $ Set.toList si

runPt1 = findComponents (Set.fromList [Ingredient "FUEL" 1] )<$> readData