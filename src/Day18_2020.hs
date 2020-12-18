module Day18_2020
    (
    ) where
    import Text.ParserCombinators.Parsec
    import Data.Either.Combinators

    data Expr = Val Int | Add | Multiply deriving Show


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

    parseApp :: Parser [Expr]
    parseApp = do many1 (choice [try parseVal, try parseAdd, try parseMultiply])

    foldlBy2 :: [Expr] -> Expr -> Int
    foldlBy2 [x,y] z = evaluate x z y
    foldlBy2 (x:y:xs) z = foldlBy2 xs $! (Val (evaluate x z y))

    evaluateExpression (x:xs) = foldlBy2 xs x

    test input = evaluateExpression $ fromRight' $ parse parseApp "" input