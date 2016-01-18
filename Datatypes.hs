{-
-- Datatype soup
-}
module Datatypes where
{------------------------------------------------------------------------------}
-- Datatypes for LGf sequents
{------------------------------------------------------------------------------}
-- Atomic type
type Atom = String

-- Generic formula datatype
data Formula = P PFormula
  | N NFormula
  deriving (Eq)

-- Positive formula datatype
data PFormula = Positive Atom
  | Tensor Formula Formula
  | RDiff Formula Formula
  | LDiff Formula Formula
  deriving (Eq)

data NFormula = Negative Atom
  | Sum Formula Formula
  | RDiv Formula Formula
  | LDiv Formula Formula
  deriving (Eq)

-- Input structure datatype
data IStructure = IStruct Formula
  | FIStruct Formula
  | STensor IStructure IStructure
  | SRDiff IStructure OStructure
  | SLDiff OStructure IStructure
  deriving (Eq)

-- Output structure datatype
data OStructure = OStruct Formula
  | FOStruct Formula
  | SSum OStructure OStructure
  | SRDiv OStructure IStructure
  | SLDiv IStructure OStructure
  deriving (Eq)

-- Sequent datatype
data Sequent = Sequent IStructure OStructure
  deriving (Eq)

{------------------------------------------------------------------------------}
-- Datatypes for Lexicons
{------------------------------------------------------------------------------}
-- Lexical item type
type LexItem = (String, Formula)

-- Lexicon type
type Lexicon = [LexItem]
