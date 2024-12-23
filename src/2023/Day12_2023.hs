module Day12_2023 (
) where

import Data.Maybe (mapMaybe)
import Data.String.Utils (split)
import Data.List (group)
import Data.Bifunctor (bimap)

data Spring = Damaged | Operational | Unknown deriving (Eq, Show)

toSpring :: Char -> Maybe Spring
toSpring '#' = Just Damaged
toSpring '.' = Just Operational
toSpring '?' = Just Unknown
toSpring _ = Nothing

readLine :: String -> ([Spring], [Int])
readLine s = (t, l)
    where
        ws = words s
        t = mapMaybe toSpring (head ws)
        l = map read $ split "," (ws !! 1)

readData :: IO [([Spring], [Int])]
readData = map readLine . lines <$> readFile "resource/2023/day12"
-------------------------------
trimDown :: [Spring] -> [Spring]
trimDown = reverse . dropWhile (/= Operational) . reverse . takeWhile (/= Unknown)

defectList :: [Spring] -> [Int]
defectList = map snd . filter (\(s, _) -> s == Damaged) . map (\l -> (head l, length l)) . group

reduce :: ([Spring], [Int]) -> ([Spring], [Int])
reduce (ss, ns) = (drop (length ss') ss, drop (length dl) ns)
    where
        ss' = trimDown ss
        dl = defectList ss'

reduceFirstBlock :: ([Spring], [Int]) -> ([Spring], [Int])
reduceFirstBlock (ss, ns) | head ss == Unknown  && ds == head ns  = (drop (ds + 2) ss, drop 1 ns)
                          | otherwise = (ss, ns)
    where
        ds = length $ takeWhile (== Damaged) (drop 1 ss)

cutDownExample :: ([Spring], [Int]) -> ([Spring], [Int])
cutDownExample (ss, ns) = (ss'''', ns'''')
    where
        (ss', ns') = reduce (ss, ns)
        (ss'', ns'') = bimap reverse reverse $ reduce (reverse ss', reverse ns')
        (ss''', ns''') = reduceFirstBlock (ss'', ns'')
        (ss'''', ns'''') = bimap reverse reverse $ reduceFirstBlock (reverse ss''', reverse ns''')

generateUnknowns :: Int -> [[Spring]]
generateUnknowns 0 = [[]]
generateUnknowns n =  map (Damaged :) k ++ map (Operational:) k
    where
        k = generateUnknowns (n -1)

foldInUnknowns :: [Spring] -> [Spring] -> [Spring]
foldInUnknowns xs [] = xs
foldInUnknowns [] _ = []
foldInUnknowns (Unknown: xs) (u:us) = u: foldInUnknowns xs us
foldInUnknowns (x: xs) (u:us) = x: foldInUnknowns xs (u:us)

passesGrouping :: [Int] -> [Spring] -> Bool
passesGrouping ns ss = defectList ss == ns

countPossibilities :: ([Spring], [Int]) -> Int
countPossibilities (ss, ns) = length $ filter (passesGrouping ns) samples
    where
        n = length $ filter (== Unknown) ss
        unknowns = generateUnknowns n
        samples = map (foldInUnknowns ss) unknowns

runPt1 = sum . map (countPossibilities . cutDownExample) <$> readData --HORRIBLE HORRIBLE performance, needs more intelligent filtering upfront

--------------
--What you want is how many spaces and how many blocks,the answer is in combinatorics


-------------------

{- There's just one problem - many of the springs have fallen into disrepair, so they're not actually sure which springs would even be safe to use! Worse yet, their condition records of which springs are damaged (your puzzle input) are also damaged! You'll need to help them repair the damaged records.

In the giant field just outside, the springs are arranged into rows. For each row, the condition records show every spring and whether it is operational (.) or damaged (#). This is the part of the condition records that is itself damaged; for some springs, it is simply unknown (?) whether the spring is operational or damaged.

However, the engineer that produced the condition records also duplicated some of this information in a different format! After the list of springs for a given row, the size of each contiguous group of damaged springs is listed in the order those groups appear in the row. This list always accounts for every damaged spring, and each number is the entire size of its contiguous group (that is, groups are always separated by at least one operational spring: #### would always be 4, never 2,2).

So, condition records with no unknown spring conditions might look like this:

#.#.### 1,1,3
.#...#....###. 1,1,3
.#.###.#.###### 1,3,1,6
####.#...#... 4,1,1
#....######..#####. 1,6,5
.###.##....# 3,2,1

However, the condition records are partially damaged; some of the springs' conditions are actually unknown (?). For example:

???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1

Equipped with this information, it is your job to figure out how many different arrangements of operational and broken springs fit the given criteria in each row.

In the first line (???.### 1,1,3), there is exactly one way separate groups of one, one, and three broken springs (in that order) can appear in that row: the first three unknown springs must be broken, then operational, then broken (#.#), making the whole row #.#.###.

The second line is more interesting: .??..??...?##. 1,1,3 could be a total of four different arrangements. The last ? must always be broken (to satisfy the final contiguous group of three broken springs), and each ?? must hide exactly one of the two broken springs. (Neither ?? could be both broken springs or they would form a single contiguous group of two; if that were true, the numbers afterward would have been 2,3 instead.) Since each ?? can either be #. or .#, there are four possible arrangements of springs.

The last line is actually consistent with ten different arrangements! Because the first number is 3, the first and second ? must both be . (if either were #, the first number would have to be 4 or higher). However, the remaining run of unknown spring conditions have many different ways they could hold groups of two and one broken springs:

?###???????? 3,2,1
.###.##.#...
.###.##..#..
.###.##...#.
.###.##....#
.###..##.#..
.###..##..#.
.###..##...#
.###...##.#.
.###...##..#
.###....##.#

In this example, the number of possible arrangements for each row is:

    ???.### 1,1,3 - 1 arrangement
    .??..??...?##. 1,1,3 - 4 arrangements
    ?#?#?#?#?#?#?#? 1,3,1,6 - 1 arrangement
    ????.#...#... 4,1,1 - 1 arrangement
    ????.######..#####. 1,6,5 - 4 arrangements
    ?###???????? 3,2,1 - 10 arrangements

Adding all of the possible arrangement counts together produces a total of 21 arrangements.

For each row, count all of the different arrangements of operational and broken springs that meet the given criteria. What is the sum of those counts?

Your puzzle answer was 7753.

The first half of this puzzle is complete! It provides one gold star: *
--- Part Two ---

As you look out at the field of springs, you feel like there are way more springs than the condition records list. When you examine the records, you discover that they were actually folded up this whole time!

To unfold the records, on each row, replace the list of spring conditions with five copies of itself (separated by ?) and replace the list of contiguous groups of damaged springs with five copies of itself (separated by ,).

So, this row:

.# 1

Would become:

.#?.#?.#?.#?.# 1,1,1,1,1

The first line of the above example would become:

???.###????.###????.###????.###????.### 1,1,3,1,1,3,1,1,3,1,1,3,1,1,3

In the above example, after unfolding, the number of possible arrangements for some rows is now much larger:

    ???.### 1,1,3 - 1 arrangement
    .??..??...?##. 1,1,3 - 16384 arrangements
    ?#?#?#?#?#?#?#? 1,3,1,6 - 1 arrangement
    ????.#...#... 4,1,1 - 16 arrangements
    ????.######..#####. 1,6,5 - 2500 arrangements
    ?###???????? 3,2,1 - 506250 arrangements

After unfolding, adding all of the possible arrangement counts together produces 525152.

Unfold your condition records; what is the new sum of possible arrangement counts?
 -}