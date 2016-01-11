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
-- Sample Ax 1 - supposed to succeed
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
-- Sample Ax 2 - supposed to fail
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
-- Sample Ax 3 - supposed to fail
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
-- Ax test list
axTests :: [Test]
axTests = [ax_1,ax_2,ax_3]
