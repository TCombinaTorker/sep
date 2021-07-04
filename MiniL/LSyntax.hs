-------------------------------------------------------------
-- Data-Type for L-Syntax
-------------------------------------------------------------
module LSyntax where


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

data Neg = Neg String
    deriving (Show, Eq)
    
data Goal = Goal [Literal]
    deriving (Show, Eq)

data Clause = Fact Atom | Rule Atom Goal | EmptyClause
    deriving (Show, Eq)

data Program = Prg [Clause] Goal
    deriving (Show, Eq)
