{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
module Enviroment where
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import LSyntax


-------------------------------------------------------------
-- Data Type for Enviroment and its stored Code
-------------------------------------------------------------

--e.g. the enviroment from p.128/129 with their indices looks like so, 
--     where each Map-type in our list is the translation of a prgclause or the goal
-- env = 
--    Env [ 
--          fromList [(0, unify p), (1, backtrack), (2, push q), (3, call), (4, backtrack), (5,return)], 
--          fromList [(6,unify q), (7, backtrack),(8, push r),...],
--          ...
--        ]

data EnvCode = Return | Backtrack | Prompt | Call | Unify Atom | Push Atom
    deriving (Show, Eq)

data Env = Env [Map.Map Int EnvCode] deriving (Show, Eq)
  
  
-------------------------------------------------------------
-- Environment access functions: c_first, c_next(i),... p.125
-------------------------------------------------------------

firstC :: Env -> (Int, EnvCode)
firstC (Env (x:xs)) =(0, befehl) where
    befehl :: EnvCode
    befehl = fromJust $ Map.lookup 0 x

nextC :: Env -> Int -> (Int, EnvCode)
nextC (Env env) n   = Map.findMin clause where
    clause :: Map.Map Int EnvCode
    clause = env !!   (inWhichList (Env env) n + 1)

goalC :: Env -> (Int, EnvCode)
goalC (Env env)     = Map.findMin $ last env

lastC :: Env -> (Int, EnvCode)
lastC (Env env)     = Map.findMax $ last env

-------------------------------------------------------------
-- Auxiliary functions for environment access functions
-------------------------------------------------------------

inWhichList :: Env -> Int -> Int
inWhichList (Env (x:xs)) n = inWhichList' (Env (x:xs)) n 0 where
    inWhichList' :: Env -> Int -> Int -> Int
    inWhichList' (Env (x:xs)) n k
      | Map.member n x = k
      | otherwise  = inWhichList' (Env xs) n (k+1)

fromEnv :: Env -> [Map.Map Int EnvCode] -- simmilar functionality as fromJust :: Maybe a -> a
fromEnv (Env env) = env

--minimalIndex :: Map.Map k l-> k -- TODO: this would've to be of type (Env [Map.Map k l]
--minimalIndex = Map.lookup 0 Map.keys

--maximalIndex :: Map.Map k l-> k -- TODO: this would've to be of type (Env [Map.Map k l]
--maximalIndex = last . Map.keys


-------------------------------------------------------------
-- The translation Function: Program => Environment
-------------------------------------------------------------

translate :: Program -> Env
translate x = Env (translate' x) where
    translate' :: Program -> [Map.Map Int EnvCode]
    translate' (Prg [] goal)      = furtherMap [translGoal goal]
    translate' (Prg clauses goal) = furtherMap alles where
        alles = (map translClause clauses) ++ [(translGoal goal)]

translGoal :: Goal -> [EnvCode]
translGoal (Goal [])           = []
translGoal (Goal ((Pos x):xs)) = [Push x, Call, Backtrack]++(translGoal (Goal xs))

translClause :: Clause -> [EnvCode]
translClause (Fact p)   = [Unify p, Backtrack, Return]
translClause (Rule p q) = [Unify p, Backtrack]++(translGoal q) ++ [Return]

addPrompt :: [[EnvCode]] -> [[EnvCode]]
addPrompt xs = clauses xs ++ [addPrompt' xs] where
    clauses xs = take (length xs -1) xs
    addPrompt' :: [[EnvCode]] -> [EnvCode]
    addPrompt' xs =  (last xs) ++ [Prompt]


-------------------------------------------------------------
-- Gets lists of Instructions lists [[Instruction]] and 
-- gives us them in a Data.Map.Map form as below
-- furtherMap ["no","Hello", "World"] => fmap further Data.Map.fromList [[(0,"n"),(1,"o")],[(2,"H"),(3,"e"),(4,"l"),(5,"l"),(6,"o")], [(7,"W")...]]
-------------------------------------------------------------

furtherMap :: [[EnvCode]] -> [Map.Map Int EnvCode]
furtherMap = assocListToMap . codesToAssocList . addPrompt where
    assocListToMap :: [[(Int, EnvCode)]] -> [Map.Map Int EnvCode]
    assocListToMap []  =  [Map.fromList []]
    assocListToMap x   =  (fmap Map.fromList x)
    codesToAssocList :: [[EnvCode]] -> [[(Int, EnvCode)]]
    codesToAssocList = codesToAssocList' 0 where
    codesToAssocList' :: Int -> [[EnvCode]] -> [[(Int, EnvCode)]]
    codesToAssocList' _ []     = []
    codesToAssocList' n (x:xs) = (codesToAssocList'' n x) : (codesToAssocList' (len +1) xs) where

        len = fst $ last (codesToAssocList'' n x)

        codesToAssocList'' :: Int -> [EnvCode] -> [(Int, EnvCode)]
        codesToAssocList'' n [] = []
        codesToAssocList'' n (x:xs) = (n,x) : (codesToAssocList'' (n+1) xs)

-------------------------------------------------------------
-- Example-Program from p.128. With function translation one
-- can check that it creates same enviroment as script!
-------------------------------------------------------------

fstC :: Clause
fstC = Rule (Atom (Pred "p") []) (Goal [Pos (Atom (Pred "q") [])])

secondC :: Clause
secondC = Rule (Atom (Pred "q") []) (Goal [Pos (Atom (Pred "r") [])])

thirdC :: Clause
thirdC = Fact (Atom (Pred "r") [])

goal :: Goal
goal = Goal [Pos (Atom (Pred "p") []), Pos (Atom (Pred "r") [])]

prg :: Program
prg = Prg [fstC, secondC, thirdC] goal

main :: IO ()
main = do
    print $ translate prg