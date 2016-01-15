{-
-- LoGoff system
-}

module Logoff where

import Datatypes

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
