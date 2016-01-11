{-
-- LoGoff testcases and testing functions
-- Actual unit testing frameworks are for scrubs
-}

import Logoff
import Datatypes
import Control.Monad
import System.Exit

{------------------------------------------------------------------------------}
-- Framework operative block
{------------------------------------------------------------------------------}
-- Dummy main function
main :: IO ()
main = do
  result <- ioize res
  unless (evaluateResult res) exitFailure
  where res = map evaluateTest axTests

-- Testing datatype
type Test = (Bool,Bool,String)

-- Result datatype
type Result = (Bool,String)

-- Test evaluator function (for verbose output)
evaluateTest :: Test -> Result
evaluateTest (t,i,l) = if t == i
  then (True, l ++ "... Pass")
  else (False,l ++ "... FAIL")

-- String compactor and IOizer
ioize :: [Result] -> IO [()]
ioize = mapM (putStrLn . snd)

-- Full run evaluator (for correct output)
evaluateResult :: [Result] -> Bool
evaluateResult = all fst

-- Master test list
tests :: [Test]
tests = axTests

{------------------------------------------------------------------------------}
-- Axiom block
{------------------------------------------------------------------------------}
-- Sample Ax 1 - Trivial, correct, atomic Ax-axiom
ax_1 :: Test
ax_1 = (ax_1_test, ax_1_intendedOutcome, ax_1_label)

ax_1_label :: String
ax_1_label = "Trivial Ax-1"

ax_1_intendedOutcome :: Bool
ax_1_intendedOutcome = True

ax_1_test :: Bool
ax_1_test = isAx (Sequent (IStructure (Positive (Atomic 'x')))
  (OStructureF (Positive (Atomic 'x'))))
{------------------------------------------------------------------------------}
-- Sample Ax 2 - Simple, non-matching atomic Ax-axiom
ax_2 :: Test
ax_2 = (ax_2_test, ax_2_intendedOutcome, ax_2_label)

ax_2_label :: String
ax_2_label = "Trivial Ax-2"

ax_2_intendedOutcome :: Bool
ax_2_intendedOutcome = False

ax_2_test :: Bool
ax_2_test = isAx (Sequent (IStructure (Positive (Atomic 'x')))
  (OStructureF (Positive (Atomic 'y'))))
{------------------------------------------------------------------------------}
-- Sample Ax 3 - Simple, non-focused, matching Ax-axiom
ax_3 :: Test
ax_3 = (ax_3_test, ax_3_intendedOutcome, ax_3_label)

ax_3_label :: String
ax_3_label = "Trivial Ax-3"

ax_3_intendedOutcome :: Bool
ax_3_intendedOutcome = False

ax_3_test :: Bool
ax_3_test = isAx (Sequent (IStructure (Positive (Atomic 'x')))
  (OStructure (Positive (Atomic 'x'))))
{------------------------------------------------------------------------------}
-- Sample Ax 4 - Simple, atom-to-non-atom Ax-axiom
ax_4 :: Test
ax_4 = (ax_4_test, ax_4_intendedOutcome, ax_4_label)

ax_4_label :: String
ax_4_label = "Trivial Ax-4"

ax_4_intendedOutcome :: Bool
ax_4_intendedOutcome = False

ax_4_test :: Bool
ax_4_test = isAx (Sequent (IStructure (Positive (Atomic 'x')))
  (OStructureF (Positive (Tensor (Atomic 'x') (Atomic 'x')))))
{------------------------------------------------------------------------------}
-- Sample CoAx 1 - Trivial, correct, atomic CoAx-axiom
coAx_1 :: Test
coAx_1 = (coAx_1_test, coAx_1_intendedOutcome, coAx_1_label)

coAx_1_label :: String
coAx_1_label = "Trivial CoAx-1"

coAx_1_intendedOutcome :: Bool
coAx_1_intendedOutcome = True

coAx_1_test :: Bool
coAx_1_test = isAx (Sequent (IStructureF (Negative (Atomic 'x')))
  (OStructure (Negative (Atomic 'x'))))
{------------------------------------------------------------------------------}
-- Sample CoAx 2 - Simple, non-matching atomic Ax-axiom
coAx_2 :: Test
coAx_2 = (coAx_2_test, coAx_2_intendedOutcome, coAx_2_label)

coAx_2_label :: String
coAx_2_label = "Trivial CoAx-2"

coAx_2_intendedOutcome :: Bool
coAx_2_intendedOutcome = True

coAx_2_test :: Bool
coAx_2_test = isAx (Sequent (IStructureF (Negative (Atomic 'x')))
  (OStructure (Negative (Atomic 'y'))))
{------------------------------------------------------------------------------}
-- Sample CoAx 3 - Simple, non-focused, matching Ax-axiom
coAx_3 :: Test
coAx_3 = (coAx_3_test, coAx_3_intendedOutcome, coAx_3_label)

coAx_3_label :: String
coAx_3_label = "Trivial CoAx-3"

coAx_3_intendedOutcome :: Bool
coAx_3_intendedOutcome = True

coAx_3_test :: Bool
coAx_3_test = isAx (Sequent (IStructure (Negative (Atomic 'x')))
  (OStructure (Negative (Atomic 'x'))))
{------------------------------------------------------------------------------}
-- Sample CoAx 4 - Simple, atom-to-non-atom Ax-axiom
coAx_4 :: Test
coAx_4 = (coAx_4_test, coAx_4_intendedOutcome, coAx_4_label)

coAx_4_label :: String
coAx_4_label = "Trivial CoAx-4"

coAx_4_intendedOutcome :: Bool
coAx_4_intendedOutcome = False

coAx_4_test :: Bool
coAx_4_test = isAx (Sequent (IStructureF (Negative (Atomic 'x')))
  (OStructure (Positive (Tensor (Atomic 'x') (Atomic 'x')))))
{------------------------------------------------------------------------------}
-- Ax test list
axTests :: [Test]
axTests = [ax_1,ax_2,ax_3,ax_4,coAx_1,coAx_2,coAx_3,coAx_4]
