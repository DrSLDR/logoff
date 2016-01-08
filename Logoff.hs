{-
-- LoGoff system
-}

module Logoff where

import Datatypes

-- isAx verifies if a sequent is an Ax-type axiom
isAx :: Sequent -> Bool
isAx (Sequent (IStructure (Positive inf)) (OStructureF (Positive ouf))) =
  ouf == inf
isAx _ = False
