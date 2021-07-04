import Data.Char(isUpper, isLower)



-------------------------------------------------------------
-- Data-Type for Tokens and tokenize :: String -> [Token]
-------------------------------------------------------------
data Token =
    Not | If | Point | Comma | LPar | RPar | Name String | Var String
        deriving (Eq, Show)

-- Turns String into a list of tokens
tokenize :: String -> [Token]
tokenize xs = map toToken (tokeniz3 xs) where
    tokeniz3 :: String -> [String]
    tokeniz3 xs
        | xs        == ""     = []
        | head xs   == ' '    = tokeniz3 (dropWhile (== ' ') xs)
        | take 4 xs == "not " = "not" : (tokeniz3 (drop 4 xs))
        | take 2 xs == ":-"   = ":-"  : (tokeniz3 (drop 2 xs))
        | head   xs == ','    = ","   : (tokeniz3 (tail xs))
        | head   xs == ')'    = ")"   : (tokeniz3 (tail xs))
        | head   xs == '('    = "("   : (tokeniz3 (tail xs))
        | head   xs == '.'    = "."   : (tokeniz3 (tail xs))
        | isUpper (head xs)   = 
            (takeWhile (\x -> not (elem x ",) ")) xs) 
                : (tokeniz3 (dropWhile (\x -> not (elem x ",)")) xs))
        | (isLower (head xs)) && (take 2 (tail xs) == ":-") =
            (takeWhile (/= ':') xs)
                : (tokeniz3 (dropWhile (/= ':') xs))
        | (isLower (head xs)) =
            (takeWhile (\x -> not (elem x "(),. ")) xs)
                : (tokeniz3 (dropWhile (\x -> not (elem x "(),. ")) xs))

    toToken :: String -> Token 
    toToken ""     = error "Called tokenize on empty String"
    toToken "not"  = Not
    toToken ":-"   = If
    toToken "."    = Point
    toToken ","    = Comma
    toToken "("    = LPar
    toToken ")"    = RPar
    toToken xs      
        | isUpper (head xs) = Var xs
        | otherwise         = Name xs 


-------------------------------------------------------------
-- Funcs corresponding to syntax-graphs solving word-problem
-------------------------------------------------------------

-- Input tokenlist is correct source code iff fProgram correctly works through it
wordProblem :: [Token] -> Bool
wordProblem xs = case fProgram $ Just xs of
    Just [] -> True
    Nothing -> False

fProgram :: Maybe [Token] -> Maybe [Token]
fProgram (Just (x:xs)) = case x of
    If     -> fGoal (Just (x:xs))
    Name _ -> fProgram $ fClause $ Just (x:xs)
fProgram  _   = Nothing 

fClause :: Maybe [Token] -> Maybe [Token]
fClause (Just xs) = case fNVLT (Just xs) of
    Just (Point:ys) -> Just ys
    Just (If:ys)    -> fGoal $ Just (If:ys)
    _               -> Nothing
fclause Nothing = Nothing 

fNVLT :: Maybe [Token] -> Maybe [Token]
fNVLT (Just ((Name _):xs))
    | head xs /= LPar = Just xs
    | otherwise       = func (fLTerm (Just (tail xs))) where
        func :: Maybe [Token] -> Maybe [Token]
        func (Just (RPar:ys)) = Just ys
        func (Just (Comma:ys)) = func (fLTerm (Just ys))
        func                _ = Nothing
fNVLT                 _ = Nothing

fLTerm :: Maybe [Token] -> Maybe [Token]
fLTerm (Just ((Var str):xs)) = Just xs
fLTerm (Just xs)             = fNVLT $ Just xs
fLTerm Nothing               = Nothing

-- Syntax-Graph for Literal: 
-- ------------[Lterm]-------->
--   |      ^|
--   --(Not)--
fLiteral :: Maybe [Token] -> Maybe [Token]
fLiteral (Just (Not:xs)) = fLTerm $ Just xs
fLiteral (Just xs)       = fLTerm $ Just xs
fLiteral         Nothing = Nothing

-- Syntax-Graph for Goal:
-- --(If)-->[Literal]------------------------>(Point)---->
--                    |^                   |
--                    --[Literal]--(Comma)--
fGoal :: Maybe [Token] -> Maybe [Token]
fGoal (Just (If:xs)) = startWithPt . recurrere . fLiteral $ Just xs
    where
        startWithPt :: Maybe [Token] -> Maybe [Token]
        startWithPt (Just (Point:ys)) = Just ys
        startWithPt                 _ = Nothing

        recurrere :: Maybe [Token] -> Maybe [Token]
        recurrere Nothing       = Nothing
        recurrere (Just (x:xs)) = case x of
            Comma -> recurrere $ fLiteral $ Just xs
            _     -> Just (x:xs)
fGoal              _ = Nothing
