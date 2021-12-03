module Day04_2015
    (
    ) where

    import qualified Crypto.Hash.MD5 as MD5

    import qualified Data.ByteString as B
    import qualified Data.Text as T
    import Data.Text.Encoding (encodeUtf8)
    import Data.ByteString.Base16
    import Data.ByteString.UTF8 as B8

    packStr :: String -> B.ByteString
    packStr = encodeUtf8 . T.pack

    hashStartWithCondition :: Int -> String -> Bool
    hashStartWithCondition noOfZeros input = replicate noOfZeros '0' == B8.toString (B8.take noOfZeros $ encode $ MD5.hash (packStr input))

    test :: Int -> String -> String
    test condition key = head $ filter (hashStartWithCondition condition) $ map (\n -> key ++ show n)  [1..]

    day04part1 = test 5 "ckczppom"
    day04part2 = test 6 "ckczppom"