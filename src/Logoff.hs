{-
-- LoGoff system
-}

module Logoff where

import Datatypes
import Data.Maybe

{------------------------------------------------------------------------------}
-- Axiom block
{------------------------------------------------------------------------------}
-- isAx verifies if a sequent is an Ax-type axiom
isAx :: Sequent -> Bool
isAx (RFocus (IStruct (P (Positive i))) (P (Positive o))) =
  o == i
isAx _ = False

-- isCoAx verifies if a sequent is a CoAx-type axiom
isCoAx :: Sequent -> Bool
isCoAx (LFocus (N (Negative i)) (OStruct (N (Negative o)))) =
  o == i
isCoAx _ = False

{------------------------------------------------------------------------------}
-- Focusing block
-- Note that if the functions can't treat the given sequent, they return it
{------------------------------------------------------------------------------}
-- Defocus right (or top-down focus right)
defocusR :: Sequent -> Sequent
defocusR (RFocus i (P o)) = Neutral i (OStruct (P o))
defocusR s = s

-- Inverse defocus right (top-down focus right)
idefocusR :: Sequent -> Sequent
idefocusR (Neutral i (OStruct (P o))) = RFocus i (P o)
idefocusR s = s

-- Defocus left (or top-down focus left)
defocusL :: Sequent -> Sequent
defocusL (LFocus (N i) o) = Neutral (IStruct (N i)) o
defocusL s = s

-- Inverse defocus left (top-down focus left)
idefocusL :: Sequent -> Sequent
idefocusL (Neutral (IStruct (N i)) o) = LFocus (N i) o
idefocusL s = s

-- Focus right (or top-down defocus right)
focusR :: Sequent -> Sequent
focusR (Neutral i (OStruct (N o))) = RFocus i (N o)
focusR s = s

-- Inverse focus right (top-down defocus right)
ifocusR :: Sequent -> Sequent
ifocusR (RFocus i (N o)) = Neutral i (OStruct (N o))
ifocusR s = s

-- Focus left (or top-down defocus left)
focusL :: Sequent -> Sequent
focusL (Neutral (IStruct (P i)) o) = LFocus (P i) o
focusL s = s

-- Inverse focus left (top-down defocus left)
ifocusL :: Sequent -> Sequent
ifocusL (LFocus (P i) o) = Neutral (IStruct (P i)) o
ifocusL s = s

{------------------------------------------------------------------------------}
-- Monotonicity block
-- Note that if the functions can't treat the given sequent, they return the
-- left one
{------------------------------------------------------------------------------}
-- Tensor - introduces tensor
monoTensor :: Sequent -> Sequent -> Sequent
monoTensor (RFocus xi xo) (RFocus yi yo) =
  RFocus (STensor xi yi) (P (Tensor xo yo))
monoTensor l r = l

-- Sum - introduces sum
monoSum :: Sequent -> Sequent -> Sequent
monoSum (LFocus xi xo) (LFocus yi yo) = LFocus (N (Sum xi yi)) (SSum xo yo)
monoSum l r = l

-- Left division - introduces LDiv
monoLDiv :: Sequent -> Sequent -> Sequent
monoLDiv (RFocus xi xo) (LFocus yi yo) = LFocus (N (LDiv xo yi)) (SLDiv xi yo)
monoLDiv l r = l

-- Right division - introduces RDiv
monoRDiv :: Sequent -> Sequent -> Sequent
monoRDiv (LFocus yi yo) (RFocus xi xo) = LFocus (N (RDiv yi xo)) (SRDiv yo xi)
monoRDiv l r = l

-- Left difference - introduces LDiff
monoLDiff :: Sequent -> Sequent -> Sequent
monoLDiff (LFocus yi yo) (RFocus xi xo) =
  RFocus (SLDiff yo xi) (P (LDiff yi xo))
monoLDiff l r = l

-- Right difference - introduces RDiff
monoRDiff :: Sequent -> Sequent -> Sequent
monoRDiff (RFocus xi xo) (LFocus yi yo) =
  RFocus (SRDiff xi yo) (P (RDiff xo yi))
monoRDiff l r = l

{------------------------------------------------------------------------------}
-- Inverse monotonicity block
-- Note that if the functions can't treat the given sequent, they return it
{------------------------------------------------------------------------------}
-- Tensor - removes tensor
iMonoTensor :: Sequent -> (Sequent, Sequent)
iMonoTensor (RFocus (STensor xi yi) (P (Tensor xo yo))) =
  (RFocus xi xo, RFocus yi yo)
iMonoTensor s = (s,s)

-- Sum - removes sum
iMonoSum :: Sequent -> (Sequent, Sequent)
iMonoSum (LFocus (N (Sum xi yi)) (SSum xo yo)) =
  (LFocus xi xo, LFocus yi yo)
iMonoSum s = (s,s)

-- Left division - removes LDiv
iMonoLDiv :: Sequent -> (Sequent, Sequent)
iMonoLDiv (LFocus (N (LDiv xo yi)) (SLDiv xi yo)) =
  (RFocus xi xo, LFocus yi yo)
iMonoLDiv s = (s,s)

-- Right division - removes RDiv
iMonoRDiv :: Sequent -> (Sequent, Sequent)
iMonoRDiv (LFocus (N (RDiv yi xo)) (SRDiv yo xi)) =
  (LFocus yi yo, RFocus xi xo)
iMonoRDiv s = (s,s)

-- Left difference - removes LDiff
iMonoLDiff :: Sequent -> (Sequent, Sequent)
iMonoLDiff (RFocus (SLDiff yo xi) (P (LDiff yi xo))) =
  (LFocus yi yo, RFocus xi xo)
iMonoLDiff s = (s,s)

-- Right difference - removes RDiff
iMonoRDiff :: Sequent -> (Sequent, Sequent)
iMonoRDiff (RFocus (SRDiff xi yo) (P (RDiff xo yi))) =
  (RFocus xi  xo, LFocus yi yo)
iMonoRDiff s = (s,s)

{------------------------------------------------------------------------------}
-- Residuation block
-- Note that if the functions can't treat the given sequent, they return it
{------------------------------------------------------------------------------}
-- residuate1 - Downwards R1 rule
residuate1 :: Int -> Sequent -> Sequent
residuate1 1 (Neutral x (SRDiv z y)) = Neutral (STensor x y) z
residuate1 2 (Neutral (STensor x y) z) = Neutral y (SLDiv x z)
residuate1 _ s = s

-- residuate1i - Upwards (inverted) R1 rule
residuate1i :: Int -> Sequent -> Sequent
residuate1i 2 (Neutral y (SLDiv x z)) = Neutral (STensor x y) z
residuate1i 1 (Neutral (STensor x y) z) = Neutral x (SRDiv z y)
residuate1i _ s = s

-- residuate2 - Downwards R2 rule
residuate2 :: Int -> Sequent -> Sequent
residuate2 1 (Neutral (SLDiff y z) x) = Neutral z (SSum y x)
residuate2 2 (Neutral z (SSum y x)) = Neutral (SRDiff z x) y
residuate2 _ s = s

-- residuate2i - Upwards (inverted) R2 rule
residuate2i :: Int -> Sequent -> Sequent
residuate2i 2 (Neutral (SRDiff z x) y) = Neutral z (SSum y x)
residuate2i 1 (Neutral z (SSum y x)) = Neutral (SLDiff y z) x
residuate2i _ s = s

{------------------------------------------------------------------------------}
-- Rewrite block
-- Note that if the functions can't treat the given sequent, they return it
{------------------------------------------------------------------------------}
-- rewriteL - rewrites tensor, left and right difference (structure to logical)
rewriteL :: Sequent -> Sequent
rewriteL (Neutral (STensor (IStruct x) (IStruct y)) o) =
  Neutral (IStruct (P (Tensor x y))) o
rewriteL (Neutral (SRDiff (IStruct x) (OStruct y)) o) =
  Neutral (IStruct (P (RDiff x y))) o
rewriteL (Neutral (SLDiff (OStruct x) (IStruct y)) o) =
  Neutral (IStruct (P (LDiff x y))) o
rewriteL s = s

-- rewriteR - rewrites sum, left and right division (structure to logical)
rewriteR :: Sequent -> Sequent
rewriteR (Neutral i (SSum (OStruct x) (OStruct y))) =
  Neutral i (OStruct (N (Sum x y)))
rewriteR (Neutral i (SRDiv (OStruct x) (IStruct y))) =
  Neutral i (OStruct (N (RDiv x y)))
rewriteR (Neutral i (SLDiv (IStruct x) (OStruct y))) =
  Neutral i (OStruct (N (LDiv x y)))
rewriteR s = s

-- rewriteLi - rewrites tensor, left and right difference (logical to structure)
rewriteLi :: Sequent -> Sequent
rewriteLi (Neutral (IStruct (P (Tensor x y))) o) =
  Neutral (STensor (IStruct x) (IStruct y)) o
rewriteLi (Neutral (IStruct (P (RDiff x y))) o) =
  Neutral (SRDiff (IStruct x) (OStruct y)) o
rewriteLi (Neutral (IStruct (P (LDiff x y))) o) =
  Neutral (SLDiff (OStruct x) (IStruct y)) o
rewriteLi s = s

-- rewriteRi - rewrites sum, left and right division (logical to structure)
rewriteRi :: Sequent -> Sequent
rewriteRi (Neutral i (OStruct (N (Sum x y)))) =
  Neutral i (SSum (OStruct x) (OStruct y))
rewriteRi (Neutral i (OStruct (N (RDiv x y)))) =
  Neutral i (SRDiv (OStruct x) (IStruct y))
rewriteRi (Neutral i (OStruct (N (LDiv x y)))) =
  Neutral i (SLDiv (IStruct x) (OStruct y))
rewriteRi s = s

{------------------------------------------------------------------------------}
-- Top-Down solver block
{------------------------------------------------------------------------------}
-- tdSolve - Master Top-Down solver
-- Also, yay maybe monad
tdSolve :: Sequent -> Maybe ProofTree
tdSolve s
  | isAx s = Just (Ax s)
  | isCoAx s = Just (CoAx s)
  | otherwise = case tdSolveRewrite s of
    Nothing -> case tdSolveMono s of
      Nothing -> case tdSolveFocus s of
        Nothing -> case tdSolveRes tdSolveResHelperList s of
          Nothing -> Nothing
          pt -> pt
        pt -> pt
      pt -> pt
    pt -> pt

-- tdSolveHelperSpecial - master-solver following a residuation step to prevent
-- residuation looping
tdSolveHelperSpecial :: Residuation -> Sequent -> Maybe ProofTree
tdSolveHelperSpecial res s
  | isAx s = Just (Ax s)
  | isCoAx s = Just (CoAx s)
  | otherwise = case tdSolveRewrite s of
    Nothing -> case tdSolveMono s of
      Nothing -> case tdSolveFocus s of
        Nothing -> case tdSolveRes (tdSolveResHelperPermit res) s of
          Nothing -> Nothing
          pt -> pt
        pt -> pt
      pt -> pt
    pt -> pt

-- tdSolveRewrite - solves rewriting
tdSolveRewrite :: Sequent -> Maybe ProofTree
tdSolveRewrite s = let
  list = [(rewriteLi, RewriteL), (rewriteRi, RewriteR)]
  complist = map (\(f,o) -> (f s, o)) list
  res = dropWhile (\(ns, _) -> s == ns) complist in
  case res of
    ((ns, o):_) -> case tdSolve ns of
      (Just pt) -> Just (Unary s o pt)
      otherwise -> Nothing
    [] -> Nothing

-- tdSolveFocus - solves focusing
tdSolveFocus :: Sequent -> Maybe ProofTree
tdSolveFocus s = let
  list = [(idefocusR, DeFocusR), (idefocusL, DeFocusL), (ifocusR, FocusR),
    (ifocusL, FocusL)]
  complist = map (\(f,o) -> (f s, o)) list
  res = dropWhile (\(ns, _) -> s == ns) complist in
  case res of
    ((ns, o):_) -> case tdSolve ns of
      (Just pt) -> Just (Unary s o pt)
      otherwise -> Nothing
    [] -> Nothing

-- tdSolveMono - solves inverse monotonicity
tdSolveMono :: Sequent -> Maybe ProofTree
tdSolveMono s = let
  list = [(iMonoTensor, MonoTensor), (iMonoSum, MonoSum), (iMonoLDiv, MonoLDiv),
    (iMonoRDiv, MonoRDiv), (iMonoLDiff, MonoLDiff), (iMonoRDiff, MonoRDiff)]
  complist = map (\(f,o) -> (f s, o)) list
  res = dropWhile (\((ns,_), _) -> s == ns) complist in
  case res of
    (((ns1, ns2), o):_) -> case tdSolve ns1 of
      (Just pt1) -> case tdSolve ns2 of
        (Just pt2) -> Just (Binary s o pt1 pt2)
        otherwise -> Nothing
      otherwise -> Nothing
    [] -> Nothing

-- tdSolveRes - solves residuation
tdSolveRes :: [(Sequent -> Sequent, Residuation)] -> Sequent -> Maybe ProofTree
tdSolveRes permit s = let
  complist = map (\(f, o) -> (f s, o)) permit
  residuations = [(ns, o) | (ns,o) <- complist, ns /= s] in
  case tdSolveResHelperOptions residuations of
    Nothing -> Nothing
    (Just (pt, res)) -> Just (Unary s (Res res) pt)

-- tdSolveResHelperOptions - solves residuation, handles the actual branching
tdSolveResHelperOptions :: [(Sequent, Residuation)] ->
  Maybe (ProofTree, Residuation)
tdSolveResHelperOptions [] = Nothing
tdSolveResHelperOptions ((s, res):xs) =
  case tdSolveHelperSpecial (tdSolveResHelperGetInverse res) s of
    Nothing -> tdSolveResHelperOptions xs
    (Just pt) -> Just (pt, res)

-- tdSolveResHelperPermit - calculates new permit-list
tdSolveResHelperPermit :: Residuation -> [(Sequent -> Sequent, Residuation)]
tdSolveResHelperPermit res = filter (\(f, r) -> r /= res) tdSolveResHelperList

-- tdSolveResHelperGetInverse
tdSolveResHelperGetInverse :: Residuation -> Residuation
tdSolveResHelperGetInverse (Res1 i) = Res1i i
tdSolveResHelperGetInverse (Res1i i) = Res1 i
tdSolveResHelperGetInverse (Res2 i) = Res2i i
tdSolveResHelperGetInverse (Res2i i) = Res2 i

-- tdSolveResHelperList - list of functions and operation mappings
tdSolveResHelperList :: [(Sequent -> Sequent, Residuation)]
tdSolveResHelperList = [(f i,o i) | (f,o) <- [(residuate1, Res1),
  (residuate2, Res2), (residuate1i, Res1i), (residuate2i, Res2i)], i <- [1,2]]

{------------------------------------------------------------------------------}
-- Bottom-up solver block
{------------------------------------------------------------------------------}
