module Day18_2020
    (
    ) where
    import Text.ParserCombinators.Parsec
    import Data.Either.Combinators

    data Expr = Val Int | Brackets [Expr] | Add | Multiply deriving Show


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

    ----
    evaluate Add (Val x) (Val y) = x + y
    evaluate Multiply (Val x) (Val y) = x * y

    evaluateSimpleBrackets :: ([Expr] -> Int) -> Expr -> Expr
    evaluateSimpleBrackets f (Brackets contents) | null [ x | x@(Brackets _) <- contents ] = Val (f contents)
                                                 | otherwise = Brackets $! map (evaluateSimpleBrackets f) contents
    evaluateSimpleBrackets _ a = a

    flattenBrackets :: ([Expr] -> Int) -> [Expr] -> Expr
    flattenBrackets f xs | null [ x | x@(Brackets _) <- xs ] = Val (f xs)
                         | otherwise = flattenBrackets f (map (evaluateSimpleBrackets f) xs)             

    evaluateExpression :: [Expr] -> Int
    evaluateExpression (x:xs) = foldlBy2 xs x

    foldlBy2 :: [Expr] -> Expr -> Int
    foldlBy2 [] (Val n) = n
    foldlBy2 [x,y] z = evaluate x z y
    foldlBy2 (x:y:xs) z = foldlBy2 xs $! Val (evaluate x z y)

    day18Pt1 = sum . map ((\(Val n) -> n) . flattenBrackets evaluateExpression . fromRight' . parse parseApp "") <$> readSums
    -----------
    --need to walk through the expression and do the sums first
    doSumsFirst :: [Expr] -> [Expr]
    doSumsFirst [] = []
    doSumsFirst (x:Add:z:xs) = Val (evaluate Add x z) : xs
    doSumsFirst (x:Multiply:z:xs) = x:Multiply: doSumsFirst (z:xs)

    findAllSums xs | null [ x | x@Add <- xs ] = xs
                   | otherwise = findAllSums $! doSumsFirst xs

    day18Pt2 =  sum . map ((\(Val n) -> n) . flattenBrackets (evaluateExpression . findAllSums) . fromRight' . parse parseApp "") <$> readSums

    readSums :: IO [String]
    readSums = lines <$> readFile "resource/2020/day18"