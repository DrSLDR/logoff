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

-- Polarity datatype
data Polar = Positive Formula
  | Negative Formula

-- Focus datatype
data Focused = Focused Polar
