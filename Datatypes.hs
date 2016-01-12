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
  | Tensor Polar Polar
  | RDiff Polar Polar
  | LDiff Polar Polar
  | Sum Polar Polar
  | RDiv Polar Polar
  | LDiv Polar Polar
  deriving (Eq)

-- Polarity datatype
data Polar = Positive Formula
  | Negative Formula
  deriving (Eq)

-- Input structure datatype
data IStructure = IStructure Polar
  | IStructureF Polar
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
