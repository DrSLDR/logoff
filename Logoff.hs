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
isAx (Sequent (IStruct (P inf)) (OStruct  (FP ouf))) =
  ouf == inf
isAx _ = False

-- isCoAx verifies if a sequent is a CoAx-type axiom
isCoAx :: Sequent -> Bool
isCoAx (Sequent (IStruct  (FN inf)) (OStruct (N ouf))) =
  ouf == inf
isCoAx _ = False

{------------------------------------------------------------------------------}
-- Focusing block
-- Note that if the functions can't treat the given sequent, they return it
{------------------------------------------------------------------------------}
-- Defocus right (or top-down focus right)
defocusR :: Sequent -> Sequent
defocusR (Sequent i (OStruct (FP o))) =
  Sequent i (OStruct (P o))
defocusR s = s

-- Defocus left (or top-down focus left)
defocusL :: Sequent -> Sequent
defocusL (Sequent (IStruct (FN i)) o) =
  Sequent (IStruct (N i)) o
defocusL s = s

-- Focus right (or top-down defocus right)
focusR :: Sequent -> Sequent
focusR (Sequent i (OStruct (N o))) =
  Sequent i (OStruct (FN o))
focusR s = s

-- Focus left (or top-down defocus left)
focusL :: Sequent -> Sequent
focusL (Sequent (IStruct (P i)) o) =
  Sequent (IStruct (FP i)) o
focusL s = s
