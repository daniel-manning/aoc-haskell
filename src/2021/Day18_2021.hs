module Day18_2021 where


data SnailFishNumber = Pair SnailFishNumber SnailFishNumber | Value Int deriving Show



{- reduce :: SnailFishNumber -> SnailFishNumber
reduce = _


add :: SnailFishNumber -> SnailFishNumber -> SnailFishNumber
add x y = reduce $ Pair x y -}