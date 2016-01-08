{-
-- LoGoff testcases and testing functions
-- Actual unit testing frameworks are for scrubs
-}

module Testcases where

import Logoff
import Datatypes

-- Sample Ax 1 - supposed to succeed
ax_1 :: Sequent
ax_1 = Sequent (IStructure (Positive (Atomic 'x')))
  (OStructureF (Positive (Atomic 'x')))

ax_1_intendedOutcome :: Bool
ax_1_intendedOutcome = True

test_ax_1 :: Bool
test_ax_1 = isAx ax_1
{------------------------------------------------------------------------------}

-- Sample Ax 2 - supposed to fail
ax_2 :: Sequent
ax_2 = Sequent (IStructure (Positive (Atomic 'x')))
  (OStructureF (Positive (Atomic 'y')))

ax_2_intendedOutcome :: Bool
ax_2_intendedOutcome = False

test_ax_2 :: Bool
test_ax_2 = isAx ax_2
{------------------------------------------------------------------------------}

-- Sample Ax 3 - supposed to fail
ax_3 :: Sequent
ax_3 = Sequent (IStructure (Positive (Atomic 'x')))
  (OStructure (Positive (Atomic 'x')))

ax_3_intendedOutcome :: Bool
ax_3_intendedOutcome = False

test_ax_3 :: Bool
test_ax_3 = isAx ax_3
{------------------------------------------------------------------------------}
