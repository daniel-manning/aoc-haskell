module Main where

import Day02

main :: IO ()
main = putStrLn <$> show =<< totalValidPasswords
