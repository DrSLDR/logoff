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

-- Generic formula comparator function
instance Eq Formula where
  (N f1) == (N f2) = f1 == f2
  (P f1) == (P f2) = f1 == f2
  _ == _ = False

-- Positive formula datatype
data PFormula = Positive Atom
  | Tensor Formula Formula
  | RDiff Formula Formula
  | LDiff Formula Formula

-- Positive formula comparator function
instance Eq PFormula where
  (Positive a1) == (Positive a2) = a1 == a2
  (Tensor x1 y1) == (Tensor x2 y2) = (x1 == x2) && (y1 == y2)
  (RDiff x1 y1) == (RDiff x2 y2) = (x1 == x2) && (y1 == y2)
  (LDiff x1 y1) == (LDiff x2 y2) = (x1 == x2) && (y1 == y2)
  _ == _ = False

-- Negative formula datatype
data NFormula = Negative Atom
  | Sum Formula Formula
  | RDiv Formula Formula
  | LDiv Formula Formula

-- Negative formula comparator function
instance Eq NFormula where
  (Negative a1) == (Negative a2) = a1 == a2
  (Sum x1 y1) == (Sum x2 y2) = (x1 == x2) && (y1 == y2)
  (RDiv x1 y1) == (RDiv x2 y2) = (x1 == x2) && (y1 == y2)
  (LDiv x1 y1) == (LDiv x2 y2) = (x1 == x2) && (y1 == y2)
  _ == _ = False

-- Input structure datatype
data IStructure = IStruct Formula
  | FIStruct NFormula
  | STensor IStructure IStructure
  | SRDiff IStructure OStructure
  | SLDiff OStructure IStructure

-- Input structure comparator function
instance Eq IStructure where
  (IStruct f1) == (IStruct f2) = f1 == f2
  (FIStruct f1) == (FIStruct f2) = f1 == f2
  (STensor is11 is12) == (STensor is21 is22) = (is11 == is21) && (is12 == is22)
  (SRDiff is1 os1) == (SRDiff is2 os2) = (is1 == is2) && (os1 == os2)
  (SLDiff os1 is1) == (SLDiff os2 is2) = (os1 == os2) && (is1 == is2)
  _ == _ = False

-- Output structure datatype
data OStructure = OStruct Formula
  | FOStruct PFormula
  | SSum OStructure OStructure
  | SRDiv OStructure IStructure
  | SLDiv IStructure OStructure

-- Output structure comparator function
instance Eq OStructure where
  (OStruct f1) == (OStruct f2) = f1 == f2
  (FOStruct f1) == (FOStruct f2) = f1 == f2
  (SSum os11 os12) == (SSum os21 os22) = (os11 == os21) && (os12 == os22)
  (SRDiv os1 is1) == (SRDiv os2 is2) = (os1 == os2) && (is1 == is2)
  (SLDiv is1 os1) == (SLDiv is2 os2) = (is1 == is2) && (os1 == os2)
  _ == _ = False

-- Sequent datatype
data Sequent = Sequent IStructure OStructure

-- Sequent comparator function
instance Eq Sequent where
  (Sequent is1 os1) == (Sequent is2 os2) = (is1 == is2) && (os1 == os2)
  _ == _ = False

{------------------------------------------------------------------------------}
-- Datatypes for Lexicons
{------------------------------------------------------------------------------}
-- Lexical item type
type LexItem = (String, Formula)

-- Lexicon type
type Lexicon = [LexItem]
