module Mbnf.Parser
    ( parseMbnf
    ) where

import Control.Exception
import Model.Mbnf (Category(Category), Expr(Expr), Lhs(Lhs), Mbnf(Mbnf),
    Rhs(Rhs), Rule(Rule), Symbol(NonTerminal, Terminal))
import Model.Path (Path(Path))
import System.IO
import Text.Parsec (Parsec,
    (<|>), char, letter, many1, parse, sepBy, spaces, string)

parseMbnf :: String -> IO Mbnf
parseMbnf path = do
    putStrLn $ "Compiling " ++ show path
    cParse path
    
cParse :: String -> IO Mbnf
cParse path =
    do
        bracket (openFile path ReadMode) hClose $
            (\h ->
                do
                    c <- hGetContents h
                    foldr (\x _ -> return x) (error "bang!") (
                        parse (mbnfParser path) "" c))

mbnfParser :: String -> Parsec String () Mbnf
mbnfParser path = spaces *> pMbn path <* spaces

pMbn :: String -> Parsec String () Mbnf
pMbn path = fmap (\rs ->
    (Mbnf (Path (takeWhile (\c ->
        c /= '.') path)) (rs)
    )) (sepBy pRul (char '\n')) 


pRul :: Parsec String () Rule
pRul =
    do
        l <- spaces *> pLhs <* spaces
        string "::="
        r <- spaces *> pRhs <* spaces
        return $ Rule l r

pLhs :: Parsec String () Lhs
pLhs =
    do
        c <- pCat
        spaces
        s <- pSym
        return $ Lhs c s

pRhs :: Parsec String () Rhs
pRhs = fmap Rhs (sepBy pExp (char '|'))

pCat :: Parsec String () Category
pCat = fmap Category (many1 letter)

pSym :: Parsec String () Symbol
pSym = (fmap Terminal ( (char '"') *> (many1 letter) <* (char '"'))) <|>
    (fmap NonTerminal (many1 letter))

pExp :: Parsec String () Expr
pExp = fmap Expr pSym