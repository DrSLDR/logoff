{--
-- The Almighty Parser
-- Disclaimer: Parser is not that mighty
-- Parser is also heavily based on work by Hutton and Meijer. My hat goes off to
-- them for that.
--}
module TAP
  (niceRead)
  where

import Data.Char
import Datatypes
{------------------------------------------------------------------------------}
-- Monad class extensions
{------------------------------------------------------------------------------}
class Monad m => MonadZero m where
  zero :: m a

class MonadZero m => MonadPlus m where
  (+.+) :: m a -> m a -> m a

{------------------------------------------------------------------------------}
-- The Almighty Parser monad itself
{------------------------------------------------------------------------------}
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

{------------------------------------------------------------------------------}
-- Parser construction functions
{------------------------------------------------------------------------------}
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

-- sepby runs one parser separated by some other parser. The result of the
-- second parser is discarded
sepby :: Parser a -> Parser b -> Parser [a]
p `sepby` sep = (p `sepby1` sep) +++ return []

-- sepby1, similar to many1, allows one or more applications of the sep-parser
sepby1 :: Parser a -> Parser b -> Parser [a]
p `sepby1` sep = do
  a <- p
  as <- many (do {sep; p})
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
apply p = parse (do {space; p})

{------------------------------------------------------------------------------}
-- NiceRead extensions
{------------------------------------------------------------------------------}
-- niceRead parses the given string using the sequent parser and returns the
-- sequent
niceRead :: String -> Sequent
niceRead = extract . apply sequent

-- extraction function gets the relevant value from the parser
extract :: [(a,String)] -> a
extract [] = error "Parser crash!"
extract l = (fst . head) l

-- sequent is the top-level sequent parser
sequent :: Parser Sequent
sequent = Parser (\cs -> return (Sequent (IStruct (P (Positive "y"))) (OStruct (P (Positive "y"))),cs))

-- atom parses single- or multi-letter atoms and their polarity
atom :: Parser Formula
atom = Parser (\cs -> case apply (many1 (sat isAlphaNum)) cs of
  [] -> []
  ((a,res):_) -> case apply polarity res of
    [] -> []
    ((p,res):_) -> case p of
      "+" -> [(P (Positive a), res)]
      "-" -> [(N (Negative a), res)])

-- polarity is an atom-level parser to parse the polarity flags
polarity :: Parser String
polarity = symb "+" +++ symb "-"

-- formula parses non-structural (nested) formulas, both positive and negative
formula :: Parser Formula
formula = Parser (\cs -> case apply (nested formula +++ atom) cs of
  [] -> []
  ((l,res):_) -> case apply connective res of
    [] -> []
    ((c,res):_) -> case apply (nested formula +++ atom) res of
      [] -> []
      ((r,res):_) -> case c of
        "(x)" -> [(P (Tensor l r),res)]
        "(/)" -> [(P (RDiff l r),res)]
        "(\\)"-> [(P (LDiff l r),res)]
        "(+)" -> [(N (Sum l r),res)]
        "/"   -> [(N (RDiv l r),res)]
        "\\"  -> [(N (LDiv l r),res)])

-- connective is a formula-level parser to parse (logical) connectives
connective :: Parser String
connective = (foldl1 (+++) . map symb) ["(+)","(x)","(/)","(\\)","/","\\"]

-- bracket is a parser-builder to determine if an input contains something
-- inside brackets
bracket :: Parser a -> Parser b -> Parser c -> Parser b
bracket open p close = do
  open
  out <- p
  close
  return out

-- nested is a parser-builder to determine if an input is nested in parentheses
nested :: Parser a -> Parser a
nested p = bracket (symb "(") p (symb ")")
