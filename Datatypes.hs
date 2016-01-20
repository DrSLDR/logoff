{-
-- Datatype soup
-}
module Datatypes where

import Data.Char
{------------------------------------------------------------------------------}
-- Nice-output show class
{------------------------------------------------------------------------------}
class NiceShow a where
  niceShow :: a -> String

instance NiceShow Formula where
  niceShow (P f) = niceShow f
  niceShow (N f) = niceShow f

instance NiceShow PFormula where
  niceShow (Positive p) = p ++ "+"
  niceShow (Tensor f1 f2) = "(" ++ niceShow f1 ++ " (x) " ++ niceShow f2 ++ ")"
  niceShow (RDiff f1 f2) = "(" ++ niceShow f1 ++ " (/) " ++ niceShow f2 ++ ")"
  niceShow (LDiff f1 f2) = "(" ++ niceShow f1 ++ " (\\) " ++ niceShow f2 ++ ")"

instance NiceShow NFormula where
  niceShow (Negative p) = p ++ "-"
  niceShow (Sum f1 f2) = "(" ++ niceShow f1 ++ " (+) " ++ niceShow f2 ++ ")"
  niceShow (RDiv f1 f2) = "(" ++ niceShow f1 ++ " / " ++ niceShow f2 ++ ")"
  niceShow (LDiv f1 f2) = "(" ++ niceShow f1 ++ " \\ " ++ niceShow f2 ++ ")"

instance NiceShow IStructure where
  niceShow (IStruct f) = niceShow f
  niceShow (FIStruct f) = "[" ++ niceShow f ++ "]"
  niceShow (STensor s1 s2) =
    "(" ++ niceShow s1 ++ " .(x). " ++ niceShow s2 ++ ")"
  niceShow (SRDiff s1 s2) =
    "(" ++ niceShow s1 ++ " .(/). " ++ niceShow s2 ++ ")"
  niceShow (SLDiff s1 s2) =
    "(" ++ niceShow s1 ++ " .(\\). " ++ niceShow s2 ++ ")"

instance NiceShow OStructure where
  niceShow (OStruct f) = niceShow f
  niceShow (FOStruct f) = "[" ++ niceShow f ++ "]"
  niceShow (SSum s1 s2) = "(" ++ niceShow s1 ++ " .(+). " ++ niceShow s2 ++ ")"
  niceShow (SRDiv s1 s2) = "(" ++ niceShow s1 ++ " ./. " ++ niceShow s2 ++ ")"
  niceShow (SLDiv s1 s2) = "(" ++ niceShow s1 ++ " .\\. " ++ niceShow s2 ++ ")"

instance NiceShow Sequent where
  niceShow (Sequent is os) = niceShow is ++ " |- " ++ niceShow os

{------------------------------------------------------------------------------}
-- Nice-input read function and parser
{------------------------------------------------------------------------------}
niceRead :: String -> Sequent
niceRead _ = Sequent (IStruct (P (Positive "x"))) (OStruct (P (Positive "x")))

-- Monad class extensions
class Monad m => MonadZero m where
  zero :: m a

class MonadZero m => MonadPlus m where
  (+.+) :: m a -> m a -> m a

-- The Almighty NiceRead Parser
newtype Parser a  = Parser (String -> [(a,String)])

-- Monad instantiations
instance Monad Parser where
  return a = Parser (\cs -> [(a,cs)])
  p1 >>= p2 = Parser (\cs -> concat [parse (p2 a) cs' | (a,cs') <- parse p1 cs])

instance MonadZero Parser where
  zero = Parser (const [])

instance MonadPlus Parser where
  p1 +.+ p2 = Parser (\cs -> parse p1 cs ++ parse p2 cs)

-- Deterministic choice
(+++) :: Parser a -> Parser a -> Parser a
p1 +++ p2 = Parser (\cs -> case parse (p1 +.+ p2) cs of
  [] -> []
  (x:xs) -> [x])

-- Parser function
parse :: Parser a -> String -> [(a,String)]
parse (Parser p) = p

{- Parser construction functions ----------------------------------------------}
-- item parses the first character in a string
item :: Parser Char
item = Parser (\cs -> case cs of
  "" -> []
  (c:cs) -> [(c,cs)])

-- sat is a conditional parser which returns a parser if a test passes
sat :: (Char -> Bool) -> Parser Char
sat p = do
  c <- item
  if p c then return c else zero

-- char checks if a parsed character equals a given character
char :: Char -> Parser Char
char c = sat (c ==)

-- string recursively parses an entire string
string :: String -> Parser String
string "" = return ""
string (c:cs) = do
  char c
  string cs
  return (c:cs)

-- many allows zero or more applications of the given parser
many :: Parser a -> Parser [a]
many p = many1 p +++ return []

-- many1 allows one or more applications of the given parser
many1 :: Parser a -> Parser [a]
many1 p = do
  a <- p
  as <- many p
  return (a:as)

-- chainl parses with two parsers, the second of which parses left-associative
-- operators
chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op a = (p `chainl1` op) +++ return a

-- chainl1 complements chainl parsing
chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op = do
  a <- p
  rest a
  where rest a = (do
        f <- op
        b <- p
        rest (f a b)) +++ return a

-- space is a lexical combinator that parses whitespace
space :: Parser String
space = many (sat isSpace)

-- token parses a token and discards trailing space
token :: Parser a -> Parser a
token p = do
  a <- p
  space
  return a

-- symb parses a symbolic token
symb :: String -> Parser String
symb cs = token (string cs)

-- apply applies a parser and discards leading whitespace
apply :: Parser a -> String -> [(a,String)]
apply p = parse (do { space; p})

{------------------------------------------------------------------------------}
-- Datatypes for LGf sequents
{------------------------------------------------------------------------------}
-- Atomic type
type Atom = String

-- Generic formula datatype
data Formula = P PFormula
  | N NFormula
  deriving (Eq, Show)

-- Positive formula datatype
data PFormula = Positive Atom
  | Tensor Formula Formula
  | RDiff Formula Formula
  | LDiff Formula Formula
  deriving (Eq, Show)

data NFormula = Negative Atom
  | Sum Formula Formula
  | RDiv Formula Formula
  | LDiv Formula Formula
  deriving (Eq, Show)

-- Input structure datatype
data IStructure = IStruct Formula
  | FIStruct Formula
  | STensor IStructure IStructure
  | SRDiff IStructure OStructure
  | SLDiff OStructure IStructure
  deriving (Eq, Show)

-- Output structure datatype
data OStructure = OStruct Formula
  | FOStruct Formula
  | SSum OStructure OStructure
  | SRDiv OStructure IStructure
  | SLDiv IStructure OStructure
  deriving (Eq, Show)

-- Sequent datatype
data Sequent = Sequent IStructure OStructure
  deriving (Eq, Show)

{------------------------------------------------------------------------------}
-- Datatypes for Lexicons
{------------------------------------------------------------------------------}
-- Lexical item type
type LexItem = (String, Formula)

-- Lexicon type
type Lexicon = [LexItem]
