{-
-- Datatype soup
-}
module Datatypes where
{-
-- Datatypes for LGf sequents
-}

-- Atomic type
type Atom = Char

-- Formula datatype
-- Covers both input and output (non-structural) formulae
data Formula = Atomic Atom
  | Tensor Formula Formula
  | RDiff Formula Formula
  | LDiff Formula Formula
  | Sum Formula Formula
  | RDiv Formula Formula
  | LDiv Formula Formula
  deriving (Eq)

-- Polarity datatype
data Polar = Positive Formula
  | Negative Formula

-- Focus datatype
data Focused = Focused Polar

-- Input structure datatype
data IStructure = IStructure Polar
  | IStructureF Focused
  | STensor IStructure IStructure
  | SRDiff IStructure OStructure
  | SLDiff OStructure IStructure

-- Output structure datatype
data OStructure = OStructure Polar
  | OStructureF Polar
  | SSum OStructure OStructure
  | SRDiv OStructure IStructure
  | SLDiv IStructure OStructure

-- Sequent datatype
data Sequent = Sequent IStructure OStructure
