module Model.Mbnf
    (Category(Category), Expr(Expr), Lhs(Lhs), Mbnf(Mbnf),
        Rhs(Rhs), Rule(Rule), Symbol(NonTerminal, Terminal)
    ) where

import Model.Path (Path)

data Mbnf = Mbnf Path [Rule] deriving (Eq, Show)
data Rule = Rule Lhs Rhs deriving (Eq, Show)
data Lhs = Lhs Category Symbol deriving (Eq, Show)
data Rhs = Rhs [Expr] deriving (Eq, Show)
data Category = Category String deriving (Eq, Show)
data Symbol = NonTerminal String | Terminal String deriving (Eq, Show)
data Expr = Expr Symbol deriving (Eq, Show)

