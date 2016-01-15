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
isAx (Sequent (IStruct (P inf)) (FOStruct  ouf)) =
  ouf == inf
isAx _ = False

-- isCoAx verifies if a sequent is a CoAx-type axiom
isCoAx :: Sequent -> Bool
isCoAx (Sequent (FIStruct  inf) (OStruct (N ouf))) =
  ouf == inf
isCoAx _ = False

{------------------------------------------------------------------------------}
-- Focusing block
{------------------------------------------------------------------------------}
-- Defocus right (or top-down focus right)
defocusR :: Sequent -> Sequent
defocusR s = s

-- Defocus left (or top-down focus left)
defocusL :: Sequent -> Sequent
defocusL s = s

-- Focus right (or top-down defocus right)
focusR :: Sequent -> Sequent
focusR s = s

-- Focus left (or top-down defocus left)
focusL :: Sequent -> Sequent
focusL s = s
