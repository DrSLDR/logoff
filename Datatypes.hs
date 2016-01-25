{-
-- Datatype soup
-}
module Datatypes where

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

{------------------------------------------------------------------------------}
-- Datatypes for proof trees
{------------------------------------------------------------------------------}
-- Proof tree datatype
data ProofTree = Ax Sequent
  | CoAx Sequent
  | Unary Sequent Operation ProofTree
  | Binary Sequent Operation ProofTree ProofTree
  deriving (Show)

-- Operation datatype
data Operation = DeFocusL
  | DeFocusR
  | FocusL
  | FocusR
  | MonoTensor
  | MonoLDiff
  | MonoRDiff
  | MonoSum
  | MonoLDiv
  | MonoRDiv
  | Res1
  | Res2
  deriving (Show)
