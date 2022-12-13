module Day07_2022 (
) where

import Data.List (foldl')

data Tree = Empty | Directory String [Tree] | File Int String deriving Show
data TerminalLine = C RefinedCommand | T Tree deriving Show

data RefinedCommand = MoveUp | Cd String | Ls deriving Show

newtype Pwd = Pwd [String] deriving Show

parse :: String -> TerminalLine
parse xs | head xs == '$' = C $ parseCommand (drop 2 xs)
         | otherwise      = T $ parseTree xs

parseCommand :: String -> RefinedCommand
parseCommand xs | xs == "cd .." = MoveUp
                | xs == "ls"    = Ls
                | otherwise     = Cd (drop 3 xs)

splitAtMember :: (Eq a) => a -> [a] -> ([a], [a])
splitAtMember c xs = (k, l)
    where
        k = takeWhile (/= c) xs
        l = drop (1 + length k) xs

parseTree :: String -> Tree
parseTree t | take 3 t == "dir" = Directory (drop 4 t) []
            | otherwise = (\l -> File (read (fst l)) (snd l)) $ splitAtMember ' ' t

readData = lines <$> readFile "resource/2022/day07"

readAndParse = map parse <$> readData
---------------------
isTree :: TerminalLine -> Bool
isTree (T _) = True
isTree _ = False

stripT (T a) = a

foldUpLs :: [TerminalLine] -> [TerminalLine]
foldUpLs [] = []
foldUpLs (C (Cd n): C Ls : xs) = C (Cd n) : T (Directory n (map stripT $ takeWhile isTree xs)) : foldUpLs (dropWhile isTree xs)
foldUpLs (x : xs) = x : foldUpLs xs

directoryHasLabel :: String -> Tree -> Bool
directoryHasLabel rn (Directory n _) = rn == n
directoryHasLabel _ _ = False

replaceDirectory :: [Tree] -> Tree -> [Tree]
replaceDirectory ts (Directory rn rts) = filter (not . directoryHasLabel rn) ts ++ [Directory rn rts]


{-updateDirectoryInTree :: Tree -> (Pwd, Tree) -> Tree
updateDirectoryInTree r (Pwd ps, Empty) = r 
updateDirectoryInTree (Directory rn rts) (Pwd ps, File s n) = File s n
updateDirectoryInTree (Directory rn rts) (Pwd ps, Directory n ts) | any (directoryHasLabel rn) ts = Directory n (replaceDirectory ts (Directory rn rts))
                                                                  | otherwise = Directory n (map (\t -> updateDirectoryInTree (Directory rn rts) (Pwd ps, t)) ts)-}

updateDirectoryInTree :: Tree -> (Pwd, Tree) -> Tree
updateDirectoryInTree r (Pwd ps, Empty) = r
updateDirectoryInTree (Directory rn rts) (Pwd ps, Directory n ts) | length ps == 1 = Directory n (replaceDirectory ts (Directory rn rts))
                                                                  | otherwise = Directory n (noWork ++ work)
                                                                        where
                                                                            k = head ps
                                                                            ls = tail ps
                                                                            noWork = filter (not . directoryHasLabel k) ts
                                                                            work = map (\t -> updateDirectoryInTree (Directory rn rts) (Pwd ls, t)) (filter (directoryHasLabel k) ts)

reversePwd :: Pwd -> Pwd
reversePwd (Pwd ps) = Pwd (reverse ps)

tailPwd :: Pwd -> Pwd
tailPwd (Pwd ps) = Pwd (tail ps)

ropeEm :: (Pwd, Tree) -> TerminalLine -> (Pwd, Tree)
ropeEm (pwd, t) (C MoveUp) = ((\(Pwd ts) -> Pwd (tail ts)) pwd, t)
ropeEm (pwd, t) (C (Cd n)) = ((\(Pwd ts) -> Pwd (n : ts)) pwd, t)
--ropeEm (pwd, t) (T Empty) = (tail pwd, t)
ropeEm (pwd, t) (T (Directory n fs)) = (pwd, updateDirectoryInTree (Directory n fs) (tailPwd $ reversePwd pwd, t)) --Fix

foldUp :: [TerminalLine] -> (Pwd, Tree)
foldUp = foldl' ropeEm (Pwd [], Empty)

constructTree :: [TerminalLine] -> Tree
constructTree = snd . foldUp . foldUpLs
-----------------------------
totalSize :: Tree -> Int
totalSize Empty = 0
totalSize (Directory _ rts) = sum $ map totalSize rts
totalSize (File s _) = s

listOutDirectorySize :: Tree -> [(String, Int)]
listOutDirectorySize Empty = []
listOutDirectorySize  (File _ _) = []
listOutDirectorySize (Directory l rts) = (l, totalSize (Directory l rts)) : (listOutDirectorySize =<< rts)


runPt1 = sum . map snd . filter ((<=100000) . snd) . listOutDirectorySize . constructTree <$> readAndParse