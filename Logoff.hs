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

{------------------------------------------------------------------------------}
-- Residuation block
-- Note that if the functions can't treat the given sequent, they return it
{------------------------------------------------------------------------------}
-- residuate1 - Downwards R1 rule
residuate1 :: Sequent -> Sequent
residuate1 (Sequent x (SRDiv z y)) = Sequent (STensor x y) z
residuate1 (Sequent (STensor x y) z) = Sequent y (SLDiv x z)
residuate1 s = s

-- residuate1i - Upwards (inverted) R1 rule
residuate1i :: Sequent -> Sequent
residuate1i (Sequent y (SLDiv x z)) = Sequent (STensor x y) z
residuate1i (Sequent (STensor x y) z) = Sequent x (SRDiv z y)
residuate1i s = s

-- residuate2 - Downwards R2 rule
residuate2 :: Sequent -> Sequent
residuate2 (Sequent (SLDiff y z) x) = Sequent z (SSum y x)
residuate2 (Sequent z (SSum y x)) = Sequent (SRDiff z x) y
residuate2 s = s

-- residuate2i - Upwards (inverted) R2 rule
residuate2i :: Sequent -> Sequent
residuate2i (Sequent (SRDiff z x) y) = Sequent z (SSum y x)
residuate2i (Sequent z (SSum y x)) = Sequent (SLDiff y z) x
residuate2i s = s
