module Day18_2020
    (
    ) where
    import Text.ParserCombinators.Parsec
    import Data.Either.Combinators

    data Expr = Val Int | Brackets [Expr] | Add | Multiply deriving Show


    evaluate Add (Val x) (Val y) = x + y
    evaluate Multiply (Val x) (Val y) = x * y

    parseAdd :: Parser Expr
    parseAdd = do
        string " + "
        return Add

    parseMultiply :: Parser Expr
    parseMultiply = do
            string " * "
            return Multiply

    parseVal :: Parser Expr
    parseVal = do
           n <- many1 digit
           return $ Val (read n)

    parseBracket :: Parser Expr
    parseBracket = do
           char '('
           n <- many1 (choice [try parseBracket, try parseVal, try parseAdd, try parseMultiply])
           char ')'
           return $ Brackets n

    parseApp :: Parser [Expr]
    parseApp = do many1 (choice [try parseBracket, try parseVal, try parseAdd, try parseMultiply])

    foldlBy2 :: [Expr] -> Expr -> Int
    foldlBy2 [x,y] z = evaluate x z y
    foldlBy2 (x:y:xs) z = foldlBy2 xs $! Val (evaluate x z y)

    evaluateSimpleBrackets :: Expr -> Expr
    evaluateSimpleBrackets (Brackets contents) | null [ x | x@(Brackets _) <- contents ] = Val (evaluateExpression contents)
                                               | otherwise = Brackets $! map evaluateSimpleBrackets contents
    evaluateSimpleBrackets a = a

    flattenBrackets xs | null [ x | x@(Brackets _) <- xs ] = Val (evaluateExpression xs)
                       | otherwise = flattenBrackets (map evaluateSimpleBrackets xs)             

    evaluateExpression (x:xs) = foldlBy2 xs x

    day18Pt1 = sum . map ((\(Val n) -> n) . flattenBrackets . fromRight' . parse parseApp "") <$> readSums

    readSums :: IO [String]
    readSums = lines <$> readFile "resource/2020/day18"