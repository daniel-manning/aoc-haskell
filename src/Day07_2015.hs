module Day07_2015
    (
    ) where

    import Data.Bits
    import qualified Data.HashMap.Strict as H
    import Control.Applicative (liftA2)
    import Data.Maybe (fromJust)
    import Data.List (foldl')

    --test :: Int
    --test = 123 .&. 456


    --newtype Signals = Map String Int
    data Command = ASSIGN Int String | AND String String String | OR String String String | NOT String String | LSHIFT Int String String | RSHIFT Int String String

    example = [
            ASSIGN 123 "x",
            ASSIGN 456 "y",
            AND "x" "y" "d",
            OR "x" "y" "e",
            LSHIFT 2 "x" "f",
            RSHIFT 2 "y" "g",
            NOT "x" "h",
            NOT "y" "i"
        ]

    runCommand :: H.HashMap String Int -> Command -> H.HashMap String Int
    runCommand values (ASSIGN signal location) = H.insert location signal values
    runCommand values (OR idA idB location) = H.insert location (fromJust $ liftA2 (.|.) (H.lookup idA values) (H.lookup idB values)) (H.delete location values)
    runCommand values (AND idA idB location) = H.insert location (fromJust $ liftA2 (.&.) (H.lookup idA values) (H.lookup idB values)) (H.delete location values)
    runCommand values (LSHIFT n idA location) = H.insert location (fromJust $ flip shiftL n <$> (H.lookup idA values)) (H.delete location values)
    runCommand values (RSHIFT n idA location) = H.insert location (fromJust $ flip shiftR n <$> (H.lookup idA values)) (H.delete location values)
    runCommand values (NOT idA location) = H.insert location (fromJust $ fromInteger . complement . toInteger <$> (H.lookup idA values)) (H.delete location values)


    test = foldl' (\b a -> runCommand b a) H.empty example