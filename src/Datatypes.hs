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
  niceShow (STensor s1 s2) =
    "(" ++ niceShow s1 ++ " .(x). " ++ niceShow s2 ++ ")"
  niceShow (SRDiff s1 s2) =
    "(" ++ niceShow s1 ++ " .(/). " ++ niceShow s2 ++ ")"
  niceShow (SLDiff s1 s2) =
    "(" ++ niceShow s1 ++ " .(\\). " ++ niceShow s2 ++ ")"

instance NiceShow OStructure where
  niceShow (OStruct f) = niceShow f
  niceShow (SSum s1 s2) = "(" ++ niceShow s1 ++ " .(+). " ++ niceShow s2 ++ ")"
  niceShow (SRDiv s1 s2) = "(" ++ niceShow s1 ++ " ./. " ++ niceShow s2 ++ ")"
  niceShow (SLDiv s1 s2) = "(" ++ niceShow s1 ++ " .\\. " ++ niceShow s2 ++ ")"

instance NiceShow Sequent where
  niceShow (Neutral is os) = niceShow is ++ " |- " ++ niceShow os
  niceShow (LFocus f os) = "[" ++ niceShow f ++ "] |- " ++ niceShow os
  niceShow (RFocus is f) = niceShow is ++ " |- [" ++ niceShow f ++ "]"

instance NiceShow ProofTree where
  niceShow (Ax s) = niceShow s ++ " [Ax]"
  niceShow (CoAx s) = niceShow s ++ " [CoAx]"
  niceShow (Unary s o pt) = niceShow pt ++ " =>\n" ++ niceShow o ++ " " ++
    niceShow s
  niceShow (Binary s o pt1 pt2) = "(" ++ niceShow pt2 ++ " :R)=>\n" ++ "(" ++
    niceShow pt1 ++ " :L)=>\n" ++ niceShow o ++ " " ++ niceShow s

instance NiceShow Operation where
  niceShow o = case o of
    DeFocusL -> "[Left Defocus]"
    DeFocusR -> "[Right Defocus]"
    FocusL -> "[Left Focus]"
    FocusR -> "[Right Focus]"
    MonoTensor -> "[Tensor Monotonicity]"
    MonoLDiff -> "[Left Difference Monotonicity]"
    MonoRDiff -> "[Right Difference Monotonicity]"
    MonoSum -> "[Sum Monotonicity]"
    MonoLDiv -> "[Left Division Monotonicity]"
    MonoRDiv -> "[Right Division Monotonicity]"
    RewriteL -> "[Left Rewrite]"
    RewriteR -> "[Right Rewrite]"
    (Res res) -> case res of
      (Res1 _) -> "[Downward Residuation 1]"
      (Res1i _) -> "[Upward Residuation 1]"
      (Res2 _) -> "[Downward Residuation 2]"
      (Res2i _) -> "[Upward Residuation 2]"

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
  | STensor IStructure IStructure
  | SRDiff IStructure OStructure
  | SLDiff OStructure IStructure
  deriving (Eq, Show)

-- Output structure datatype
data OStructure = OStruct Formula
  | SSum OStructure OStructure
  | SRDiv OStructure IStructure
  | SLDiv IStructure OStructure
  deriving (Eq, Show)

-- Sequent datatype
data Sequent = Neutral IStructure OStructure
  | LFocus Formula OStructure
  | RFocus IStructure Formula
  deriving (Eq, Show)

{------------------------------------------------------------------------------}
-- Datatypes for Lexicons
{------------------------------------------------------------------------------}
-- Lexical item type
type LexItem = (String, Formula)

-- Lexicon type
type Lexicon = [LexItem]

-- Proof segment type
type PartialProof = (String, ProofTree)

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
  | RewriteL
  | RewriteR
  | Res Residuation
  deriving (Show, Eq)

-- Residuation operation datatype
data Residuation = Res1 Int
  | Res1i Int
  | Res2 Int
  | Res2i Int
  deriving (Show, Eq)

{------------------------------------------------------------------------------}
-- Testing specific datatypes
{------------------------------------------------------------------------------}
-- Testing datatype
type Test = (Bool,Bool,String)

-- Result datatype
type Result = (Bool,String)
