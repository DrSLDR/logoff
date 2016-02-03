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
isAx (Sequent (IStruct (P (Positive i))) (FOStruct  (P (Positive o)))) =
  o == i
isAx _ = False

-- isCoAx verifies if a sequent is a CoAx-type axiom
isCoAx :: Sequent -> Bool
isCoAx (Sequent (FIStruct  (N (Negative i))) (OStruct (N (Negative o)))) =
  o == i
isCoAx _ = False

{------------------------------------------------------------------------------}
-- Focusing block
-- Note that if the functions can't treat the given sequent, they return it
{------------------------------------------------------------------------------}
-- Defocus right (or top-down focus right)
defocusR :: Sequent -> Sequent
defocusR (Sequent i (FOStruct (P o))) = Sequent i (OStruct (P o))
defocusR s = s

-- Inverse defocus right (top-down focus right)
idefocusR :: Sequent -> Sequent
idefocusR (Sequent i (OStruct (P o))) = Sequent i (FOStruct (P o))
idefocusR s = s

-- Defocus left (or top-down focus left)
defocusL :: Sequent -> Sequent
defocusL (Sequent (FIStruct (N i)) o) = Sequent (IStruct (N i)) o
defocusL s = s

-- Inverse defocus left (top-down focus left)
idefocusL :: Sequent -> Sequent
idefocusL (Sequent (IStruct (N i)) o) = Sequent (FIStruct (N i)) o
idefocusL s = s

-- Focus right (or top-down defocus right)
focusR :: Sequent -> Sequent
focusR (Sequent i (OStruct (N o))) = Sequent i (FOStruct (N o))
focusR s = s

-- Inverse focus right (top-down defocus right)
ifocusR :: Sequent -> Sequent
ifocusR (Sequent i (FOStruct (N o))) = Sequent i (OStruct (N o))
ifocusR s = s

-- Focus left (or top-down defocus left)
focusL :: Sequent -> Sequent
focusL (Sequent (IStruct (P i)) o) = Sequent (FIStruct (P i)) o
focusL s = s

-- Inverse focus left (top-down defocus left)
ifocusL :: Sequent -> Sequent
ifocusL (Sequent (FIStruct (P i)) o) = Sequent (IStruct (P i)) o
ifocusL s = s

{------------------------------------------------------------------------------}
-- Monotonicity block
-- Note that if the functions can't treat the given sequent, they return the
-- left one
{------------------------------------------------------------------------------}
-- Tensor - introduces tensor
monoTensor :: Sequent -> Sequent -> Sequent
monoTensor (Sequent xi (FOStruct xo)) (Sequent yi (FOStruct yo)) =
  Sequent (STensor xi yi) (FOStruct (P (Tensor xo yo)))
monoTensor l r = l

-- Sum - introduces sum
monoSum :: Sequent -> Sequent -> Sequent
monoSum (Sequent (FIStruct xi) xo) (Sequent (FIStruct yi) yo) =
  Sequent (FIStruct (N (Sum xi yi))) (SSum xo yo)
monoSum l r = l

-- Left division - introduces LDiv
monoLDiv :: Sequent -> Sequent -> Sequent
monoLDiv (Sequent xi (FOStruct xo)) (Sequent (FIStruct yi) yo) =
  Sequent (FIStruct (N (LDiv xo yi))) (SLDiv xi yo)
monoLDiv l r = l

-- Right division - introduces RDiv
monoRDiv :: Sequent -> Sequent -> Sequent
monoRDiv (Sequent (FIStruct yi) yo) (Sequent xi (FOStruct xo)) =
  Sequent (FIStruct (N (RDiv yi xo))) (SRDiv yo xi)
monoRDiv l r = l

-- Left difference - introduces LDiff
monoLDiff :: Sequent -> Sequent -> Sequent
monoLDiff (Sequent (FIStruct yi) yo) (Sequent xi (FOStruct xo)) =
  Sequent (SLDiff yo xi) (FOStruct (P (LDiff yi xo)))
monoLDiff l r = l

-- Right difference - introduces RDiff
monoRDiff :: Sequent -> Sequent -> Sequent
monoRDiff (Sequent xi (FOStruct xo)) (Sequent (FIStruct yi) yo) =
  Sequent (SRDiff xi yo) (FOStruct (P (RDiff xo yi)))
monoRDiff l r = l

{------------------------------------------------------------------------------}
-- Inverse monotonicity block
-- Note that if the functions can't treat the given sequent, they return it
{------------------------------------------------------------------------------}
-- Tensor - removes tensor
iMonoTensor :: Sequent -> (Sequent, Sequent)
iMonoTensor (Sequent (STensor xi yi) (FOStruct (P (Tensor xo yo)))) =
  (Sequent xi (FOStruct xo), Sequent yi (FOStruct yo))
iMonoTensor s = (s,s)

-- Sum - removes sum
iMonoSum :: Sequent -> (Sequent, Sequent)
iMonoSum (Sequent (FIStruct (N (Sum xi yi))) (SSum xo yo)) =
  (Sequent (FIStruct xi) xo, Sequent (FIStruct yi) yo)
iMonoSum s = (s,s)

-- Left division - removes LDiv
iMonoLDiv :: Sequent -> (Sequent, Sequent)
iMonoLDiv (Sequent (FIStruct (N (LDiv xo yi))) (SLDiv xi yo)) =
  (Sequent xi (FOStruct xo), Sequent (FIStruct yi) yo)
iMonoLDiv s = (s,s)

-- Right division - removes RDiv
iMonoRDiv :: Sequent -> (Sequent, Sequent)
iMonoRDiv (Sequent (FIStruct (N (RDiv yi xo))) (SRDiv yo xi)) =
  (Sequent (FIStruct yi) yo, Sequent xi (FOStruct xo))
iMonoRDiv s = (s,s)

-- Left difference - removes LDiff
iMonoLDiff :: Sequent -> (Sequent, Sequent)
iMonoLDiff (Sequent (SLDiff yo xi) (FOStruct (P (LDiff yi xo)))) =
  (Sequent (FIStruct yi) yo, Sequent xi (FOStruct xo))
iMonoLDiff s = (s,s)

-- Right difference - removes RDiff
iMonoRDiff :: Sequent -> (Sequent, Sequent)
iMonoRDiff (Sequent (SRDiff xi yo) (FOStruct (P (RDiff xo yi)))) =
  (Sequent xi (FOStruct xo), Sequent (FIStruct yi) yo)
iMonoRDiff s = (s,s)

{------------------------------------------------------------------------------}
-- Residuation block
-- Note that if the functions can't treat the given sequent, they return it
{------------------------------------------------------------------------------}
-- residuate1 - Downwards R1 rule
residuate1 :: Int -> Sequent -> Sequent
residuate1 1 (Sequent x (SRDiv z y)) = Sequent (STensor x y) z
residuate1 2 (Sequent (STensor x y) z) = Sequent y (SLDiv x z)
residuate1 _ s = s

-- residuate1i - Upwards (inverted) R1 rule
residuate1i :: Int -> Sequent -> Sequent
residuate1i 2 (Sequent y (SLDiv x z)) = Sequent (STensor x y) z
residuate1i 1 (Sequent (STensor x y) z) = Sequent x (SRDiv z y)
residuate1i _ s = s

-- residuate2 - Downwards R2 rule
residuate2 :: Int -> Sequent -> Sequent
residuate2 1 (Sequent (SLDiff y z) x) = Sequent z (SSum y x)
residuate2 2 (Sequent z (SSum y x)) = Sequent (SRDiff z x) y
residuate2 _ s = s

-- residuate2i - Upwards (inverted) R2 rule
residuate2i :: Int -> Sequent -> Sequent
residuate2i 2 (Sequent (SRDiff z x) y) = Sequent z (SSum y x)
residuate2i 1 (Sequent z (SSum y x)) = Sequent (SLDiff y z) x
residuate2i _ s = s

{------------------------------------------------------------------------------}
-- Rewrite block
-- Note that if the functions can't treat the given sequent, they return it
{------------------------------------------------------------------------------}
-- rewriteL - rewrites tensor, left and right difference (structure to logical)
rewriteL :: Sequent -> Sequent
rewriteL (Sequent (STensor (IStruct x) (IStruct y)) o) =
  Sequent (IStruct (P (Tensor x y))) o
rewriteL (Sequent (SRDiff (IStruct x) (OStruct y)) o) =
  Sequent (IStruct (P (RDiff x y))) o
rewriteL (Sequent (SLDiff (OStruct x) (IStruct y)) o) =
  Sequent (IStruct (P (LDiff x y))) o
rewriteL s = s

-- rewriteR - rewrites sum, left and right division (structure to logical)
rewriteR :: Sequent -> Sequent
rewriteR (Sequent i (SSum (OStruct x) (OStruct y))) =
  Sequent i (OStruct (N (Sum x y)))
rewriteR (Sequent i (SRDiv (OStruct x) (IStruct y))) =
  Sequent i (OStruct (N (RDiv x y)))
rewriteR (Sequent i (SLDiv (IStruct x) (OStruct y))) =
  Sequent i (OStruct (N (LDiv x y)))
rewriteR s = s

-- rewriteLi - rewrites tensor, left and right difference (logical to structure)
rewriteLi :: Sequent -> Sequent
rewriteLi (Sequent (IStruct (P (Tensor x y))) o) =
  Sequent (STensor (IStruct x) (IStruct y)) o
rewriteLi (Sequent (IStruct (P (RDiff x y))) o) =
  Sequent (SRDiff (IStruct x) (OStruct y)) o
rewriteLi (Sequent (IStruct (P (LDiff x y))) o) =
  Sequent (SLDiff (OStruct x) (IStruct y)) o
rewriteLi s = s

-- rewriteRi - rewrites sum, left and right division (logical to structure)
rewriteRi :: Sequent -> Sequent
rewriteRi (Sequent i (OStruct (N (Sum x y)))) =
  Sequent i (SSum (OStruct x) (OStruct y))
rewriteRi (Sequent i (OStruct (N (RDiv x y)))) =
  Sequent i (SRDiv (OStruct x) (IStruct y))
rewriteRi (Sequent i (OStruct (N (LDiv x y)))) =
  Sequent i (SLDiv (IStruct x) (OStruct y))
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
