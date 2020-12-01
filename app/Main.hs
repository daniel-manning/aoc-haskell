module Main where

import Day01

main :: IO ()
main = putStr =<< (unlines <$> map show <$> day01Pt1Solution)
