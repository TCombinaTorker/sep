{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
import qualified Data.Map as Map
import Data.Maybe (fromJust)

-------------------------------------------------------------
-- Data-Type for L-Syntax
-------------------------------------------------------------

data Variable = Variable String
    deriving (Show, Eq)
data Term = Term Variable | TermR Func [Term]
    deriving (Show, Eq)
data Func = Func String
    deriving (Show, Eq)
data Pred = Pred String
    deriving (Show, Eq)
data Atom = Atom Pred [Term]
    deriving (Show, Eq)
data Literal = Pos Atom | Neg Atom
    deriving (Show, Eq)
data Goal = Goal [Literal]
    deriving (Show, Eq)
data Clause = Fact Atom | Rule Atom Goal | EmptyClause
    deriving (Show, Eq)
data Program = Prg [Clause] Goal
    deriving (Show, Eq)






-------------------------------------------------------------
-- Data Type for Environment
-------------------------------------------------------------
data EnvCode = Return | Backtrack | Prompt | Call | Unify Atom | Push Atom
    deriving (Show, Eq)



data Env = Env [Map.Map Int EnvCode]   deriving (Show, Eq)

--env = Env [Map.fromList [(0, Unify p), (1, backtrack), (2, push q), (3, call), (4, backtrack), (5,return)], Map.fromList[(6,unify q), (7, backtrack),(8, push r)]

-------------------------------------------------------------
-- Pre definitions for environment access functions (c_first..)
-------------------------------------------------------------

inWhichList :: Env -> Int -> Int
inWhichList (Env (x:xs)) n = inWhichList' (Env (x:xs)) n 0 where
    inWhichList' :: Env -> Int -> Int -> Int
    inWhichList' (Env (x:xs)) n k
      | Map.member n x = k
      | otherwise  = inWhichList' (Env xs) n (k+1)

fromEnv :: Env -> [Map.Map Int EnvCode] -- simmilar functionality as fromJust :: Maybe a -> a
fromEnv (Env env) = env


-------------------------------------------------------------
-- Environment access functions (c_first, c_next(i) ...)
-------------------------------------------------------------


-- c_first() from script
-------------------------------------------------------------
firstC :: Env -> (Int, EnvCode)
firstC (Env (x:xs)) =(0, befehl) where
    befehl :: EnvCode
    befehl = fromJust $ Map.lookup 0 x


--c_next(c_i)  from script
-------------------------------------------------------------
nextC :: Env -> Int -> (Int, EnvCode)
nextC (Env env) n   = Map.findMin clause where
    clause :: Map.Map Int EnvCode
    clause = env !!   (inWhichList (Env env) n + 1)

--c_goal() from script
-------------------------------------------------------------
goalC :: Env -> (Int, EnvCode)
goalC (Env env)     = Map.findMin $ last env

--c_last() from script
------------------------------------------------------------
lastC :: Env -> (Int, EnvCode)
lastC (Env env)     = Map.findMax $ last env

-------------------------------------------------------------
-- The translation Function: Program => Environment
-------------------------------------------------------------

translate :: Program -> Env
translate x = Env (translate' x) where
    translate' :: Program -> [Map.Map Int EnvCode]
    translate' (Prg [] goal)      = furtherMap [translGoal goal]
    translate' (Prg clauses goal) = furtherMap alles where
        alles = (map translClause clauses) ++ [(translGoal goal)]


-- The translation Function: Goal => [Instructions]
-------------------------------------------------------------
translGoal :: Goal -> [EnvCode]
translGoal (Goal [])           = []
translGoal (Goal ((Pos x):xs)) = [Push x, Call, Backtrack]++(translGoal (Goal xs))

-- The translation Function: Clause => [Instructions]
-------------------------------------------------------------
translClause :: Clause -> [EnvCode]
translClause (Fact p)   = [Unify p, Backtrack, Return]
translClause (Rule p q) = [Unify p, Backtrack]++(translGoal q) ++ [Return]

-- The translation Function: Goal => [Instructions]
-------------------------------------------------------------
addPrompt :: [[EnvCode]] -> [[EnvCode]]
addPrompt xs = clauses xs ++ [addPrompt' xs] where
    clauses xs = take (length xs -1) xs
    addPrompt' :: [[EnvCode]] -> [EnvCode]
    addPrompt' xs =  (last xs) ++ [Prompt]


-------------------------------------------------------------
-- Gets lists if Instructions lists [[Instruction]] and 
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


-------------------------------------------------------------
-- Data Type Registers
-------------------------------------------------------------
-- Typclassen Komponenten von stackcell
data Stack = Stack (Map.Map Int StackCell) deriving (Show, Eq)

data StackCell = EnvAdress (Maybe Int) | AtomCell Atom | StackAdress (Maybe Int)
    deriving (Eq, Show)


stackSize :: Stack -> Int
stackSize (Stack stack) = Map.size stack



newtype RegisterB = B Bool

-- return register 
newtype RegisterR = R StackCell

-- top-of-stack register, iW immer T (size stack) - 1 
newtype RegisterT = T StackCell

-- last choice point register 
newtype RegisterC = C StackCell

-- programcounter 
newtype RegisterP = P StackCell



--stack {EnvAdress (Maybe Int),  }


mainInstructCycle :: Program -> Bool
mainInstructCycle prg = undefined
-- !Data type of c Instructions can be changed by need
-- TODO: Define mainInstructionCycle 
-- TODO: Init Stacks Init Registers and
-- TODO: Bind syntax tree codes and this file
-- TODO: Define main Function with IO





plusP :: Int -> StackCell -> StackCell
plusP _ (EnvAdress Nothing) = EnvAdress Nothing
plusP x (EnvAdress (Just p)) = (EnvAdress $ Just (x + p))



push :: Atom -> RegisterC -> RegisterP -> Stack -> Env -> Stack
push atom c p stack env = Stack $ f4 (f3 (f2 (f1 stack n env) n c) n p) n atom where
    n = stackSize stack
    f1 :: Stack -> Int -> Env -> Map.Map Int StackCell
    f1 (Stack stack) n env = Map.insert (n+1) (EnvAdress $ Just $ fst (firstC env)) stack
    f2 :: Map.Map Int StackCell -> Int -> RegisterC -> Map.Map Int StackCell
    f2 map n (C c)         = Map.insert (n+2) c map
    f3 :: Map.Map Int StackCell -> Int -> RegisterP -> Map.Map Int StackCell 
    f3 map n (P p)         = Map.insert (n+3) (3 `plusP` p) map
    f4 :: Map.Map Int StackCell -> Int -> Atom -> Map.Map Int StackCell
    f4 map n atom  = Map.insert (n+4) (AtomCell atom) map






stack :: Stack
stack = Stack (Map.fromList [])

p :: RegisterP
p = P (EnvAdress Nothing)

c :: RegisterC
c = C (StackAdress Nothing)

atom :: Atom
atom = (Atom (Pred "p") [])
