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
data Formula a = P PFormula
  | N NFormula

-- Positive formula datatype
data PFormula = Positive Atom
  | Tensor (Formula a) (Formula a)
  | RDiff (Formula a) (Formula a)
  | LDiff (Formula a) (Formula a)

data NFormula = Negative Atom
  | Sum (Formula a) (Formula a)
  | RDiv (Formula a) (Formula a)
  | LDiv (Formula a) (Formula a)

-- Input structure datatype
data IStructure = IStructure (Formula a)
  | IStructureF NFormula
  | STensor IStructure IStructure
  | SRDiff IStructure OStructure
  | SLDiff OStructure IStructure

-- Output structure datatype
data OStructure = OStructure (Formula a)
  | OStructureF PFormula
  | SSum OStructure OStructure
  | SRDiv OStructure IStructure
  | SLDiv IStructure OStructure

-- Sequent datatype
data Sequent = Sequent IStructure OStructure

{------------------------------------------------------------------------------}
-- Datatypes for Lexicons
{------------------------------------------------------------------------------}
-- Lexical item type
type LexItem = (String, Polar)

-- Lexicon type
type Lexicon = [LexItem]
