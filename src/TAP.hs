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
-- Nice-input read class
{------------------------------------------------------------------------------}
class NiceRead a where
  niceRead :: String -> a

instance NiceRead Formula where
  niceRead = extract . apply formula

instance NiceRead IStructure where
  niceRead = extract . apply instructure

instance NiceRead OStructure where
  niceRead = extract . apply outstructure

instance NiceRead Sequent where
  niceRead = extract . apply sequent

{------------------------------------------------------------------------------}
-- NiceRead extensions
{------------------------------------------------------------------------------}
-- extraction function gets the relevant value from the parser
extract :: [(a,String)] -> a
extract [] = error "Parser crash!"
extract l = (fst . head) l

-- sequent is the top-level sequent parser and handles focus
sequent :: Parser Sequent
sequent = Parser (\cs -> case apply (focused formula) cs of
  ((fi,res):_) -> case apply (symb "|-") res of -- focused input
    ((_,res):_) -> case apply outstructure res of -- turnstile
      ((o,_):_) -> [(LFocus fi o,"")] -- output structure
      [] -> [] -- crash
    [] -> [] -- crash
  [] -> case apply instructure cs of -- not focused input
    ((i,res):_) -> case apply (symb "|-") res of -- input structure
      ((_,res):_) -> case apply (focused formula) res of -- turnstile
        ((fo,_):_) -> [(RFocus i fo,"")] -- focused output
        [] -> case apply outstructure res of -- not focused output
          ((o,_):_) -> [(Neutral i o,"")] -- output structure
          [] -> [] -- crash
      [] -> [] -- crash
    [] -> []) -- crash

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
    [] -> [(l,res)] -- 2 possibilities: nesting or broken connective
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

-- instructure parses input structures
instructure :: Parser IStructure
instructure = Parser (\cs -> case
  apply (instructureFormula +++ nested instructure) cs of
  ((l,res):_) -> case
    apply (symb ".(x)." +++ symb ".(/)." +++ symb ".(\\).") res of
    ((c,res):_) -> case c of
      ".(x)." -> apply (instructureTensor l) res
      ".(/)." -> apply (instructureRDiff l) res
      ".(\\)."-> apply instructureLDiff cs
    [] -> [(l, res)] -- Possibly logical structure or nested structure
  [] -> []) -- crash

-- instructureFormula is a helper function that parses logical formulas
instructureFormula :: Parser IStructure
instructureFormula = Parser (\cs -> case apply formula cs of
  ((f,res):_) -> [(IStruct f, res)]
  [] -> [])

-- instructureTensor is a helper function to parse structural tensor
instructureTensor :: IStructure -> Parser IStructure
instructureTensor l = Parser (\cs -> case
  apply (instructureFormula +++ nested instructure) cs of
  ((r,res):_) -> [(STensor l r, res)]
  [] -> [])

-- instructureRDiff is a helper function to parse structural right difference
instructureRDiff :: IStructure -> Parser IStructure
instructureRDiff l = Parser (\cs -> case apply outstructure cs of
  ((r,res):_) -> [(SRDiff l r, res)]
  [] -> [])

-- instructureLDiff is a helper function to parse structural left difference
instructureLDiff :: Parser IStructure
instructureLDiff = Parser (\cs -> case apply outstructure cs of
  ((l,res):_) -> case apply (symb ".(\\).") res of
    ((_,res):_) -> case apply (instructureFormula +++ nested instructure) res of
      ((r,res):_) -> [(SLDiff l r, res)]
      [] -> []
    [] -> []
  [] -> [])

-- outstructure starts parsing output structures and handles focus
outstructure :: Parser OStructure
outstructure = Parser (\cs -> case
  apply (outstructureFormula +++ nested outstructure) cs of
  ((l,res):_) -> case
    apply (symb ".(+)." +++ symb ".\\." +++ symb "./.") res of
    ((c,res):_) -> case c of
      ".(+)." -> apply (outstructureSum l) res
      "./."   -> apply (outstructureRDiv l) res
      ".\\."  -> apply outstructureLDiv cs
    [] -> [(l, res)] -- Possibly logical structure or nested structure
  [] -> []) -- crash

-- outstructureFormula is a helper function that parses logical formulas
outstructureFormula :: Parser OStructure
outstructureFormula = Parser (\cs -> case apply formula cs of
  ((f,res):_) -> [(OStruct f, res)]
  [] -> [])

-- outstructureSum is a helper function to parse structural sum
outstructureSum :: OStructure -> Parser OStructure
outstructureSum l = Parser (\cs -> case
  apply (outstructureFormula +++ nested outstructure) cs of
  ((r,res):_) -> [(SSum l r, res)]
  [] -> [])

-- outstructureRDiv is a helper function to parse structural right division
outstructureRDiv :: OStructure -> Parser OStructure
outstructureRDiv l = Parser (\cs -> case apply instructure cs of
  ((r,res):_) -> [(SRDiv l r, res)]
  [] -> [])

-- outstructureLDiv is a helper function to parse structural left division
outstructureLDiv :: Parser OStructure
outstructureLDiv = Parser (\cs -> case apply instructure cs of
  ((l,res):_) -> case apply (symb "./.") res of
    ((_,res):_) -> case
      apply (outstructureFormula +++ nested outstructure) res of
      ((r,res):_) -> [(SLDiv l r, res)]
      [] -> []
    [] -> []
  [] -> [])

-- focused is a parser-builder to determine id an input is focused
focused :: Parser a -> Parser a
focused p = bracket (symb "[") p (symb "]")
