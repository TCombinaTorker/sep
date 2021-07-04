import Data.Char(isUpper, isLower)
import Data.List (intercalate)
main :: IO ()
main = do
    inStr <- readFile "input.txt"
    print $ tokenize $ intercalate "" (lines inStr)


data Token =
    Not | If | Point | Comma | LPar | RPar | Name String | Var String | Fehler String
        deriving (Eq, Show)

-- Turns String into a list of tokens

tokenize :: String -> [Token]
tokenize xs
    | xs        == ""     = []
    | head xs   == ' '    = tokenize (tail xs)
    | take 4 xs == "not " = Not      : (tokenize (drop 4 xs))
    | take 2 xs == ":-"   = If       : (tokenize (drop 2 xs))
    | head   xs == ','    = Comma    : (tokenize (tail xs))
    | head   xs == ')'    = LPar     : (tokenize (tail xs))
    | head   xs == '('    = RPar     : (tokenize (tail xs))
    | head   xs == '.'    = Point    : (tokenize (tail xs))
    | isUpper (head xs)   =
            Var (takeWhile (\x -> not (elem x ",) ")) xs)
                : (tokenize (dropWhile (\x -> not (elem x ",)")) xs))
    | isLower (head xs) && (take 2 (tail xs) == ":-") =
        Name (takeWhile (/= ':') xs)
            : tokenize (dropWhile (/= ':') xs)
    | isLower (head xs) =
        Name (takeWhile (\x -> not (elem x "(),. ")) xs)
        : (tokenize (dropWhile (\x -> not (elem x "(),. ")) xs))
    | otherwise = Fehler "Unknown Token" : tokenize (tail xs)

    