{-
-- LoGoff system
-}

module Logoff where

import Datatypes

{------------------------------------------------------------------------------}
-- Axiom block
{------------------------------------------------------------------------------}
-- isAx verifies if a sequent is an Ax-type axiom
isAx :: Sequent -> Bool
isAx (Sequent (IStruct (P inf)) (FOStruct  (P ouf))) =
  ouf == inf
isAx _ = False

-- isCoAx verifies if a sequent is a CoAx-type axiom
isCoAx :: Sequent -> Bool
isCoAx (Sequent (FIStruct  (N inf)) (OStruct (N ouf))) =
  ouf == inf
isCoAx _ = False

{------------------------------------------------------------------------------}
-- Focusing block
-- Note that if the functions can't treat the given sequent, they return it
{------------------------------------------------------------------------------}
-- Defocus right (or top-down focus right)
defocusR :: Sequent -> Sequent
defocusR (Sequent i (FOStruct (P o))) =
  Sequent i (OStruct (P o))
defocusR s = s

-- Defocus left (or top-down focus left)
defocusL :: Sequent -> Sequent
defocusL (Sequent (FIStruct (N i)) o) =
  Sequent (IStruct (N i)) o
defocusL s = s

-- Focus right (or top-down defocus right)
focusR :: Sequent -> Sequent
focusR (Sequent i (OStruct (N o))) =
  Sequent i (FOStruct (N o))
focusR s = s

-- Focus left (or top-down defocus left)
focusL :: Sequent -> Sequent
focusL (Sequent (IStruct (P i)) o) =
  Sequent (FIStruct (P i)) o
focusL s = s

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
monoRDiv (Sequent xi (FOStruct xo)) (Sequent (FIStruct yi) yo) =
  Sequent (FIStruct (N (RDiv yi xo))) (SRDiv yo xi)
monoRDiv l r = l

-- Left difference - introduces LDiff
monoLDiff :: Sequent -> Sequent -> Sequent
monoLDiff (Sequent xi (FOStruct xo)) (Sequent (FIStruct yi) yo) =
  Sequent (SLDiff yo xi) (FOStruct (P (LDiff yi xo)))
monoLDiff l r = l

-- Right difference - introduces RDiff
monoRDiff :: Sequent -> Sequent -> Sequent
monoRDiff (Sequent xi (FOStruct xo)) (Sequent (FIStruct yi) yo) =
  Sequent (SRDiff xi yo) (FOStruct (P (RDiff xo yi)))
monoRDiff l r = l
