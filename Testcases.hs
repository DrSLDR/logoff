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
-- main function
main :: IO ()
main = do
  result <- ioize res
  unless (evaluateResult res) exitFailure
  where res = map evaluateTest tests

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
-- Trivial Ax-1 - Trivial, correct, atomic Ax-axiom
-- x+ |- [x+]
ax_1 :: Test
ax_1 = (isAx
  (Sequent
    (IStructure
      (Positive (Atomic "x")))
    (OStructureF
      (Positive (Atomic "x")))),
  True,
  "Trivial Ax-1"
  )
{------------------------------------------------------------------------------}
-- Trivial Ax-2 - Simple, non-matching atomic Ax-axiom
-- x+ |- [y+]
ax_2 :: Test
ax_2 = (isAx
  (Sequent
    (IStructure
      (Positive (Atomic "x")))
    (OStructureF
      (Positive (Atomic "y")))),
  False,
  "Trivial Ax-2"
  )
{------------------------------------------------------------------------------}
-- Trivial Ax-3 - Simple, non-focused, matching Ax-axiom
-- x+ |- x+
ax_3 :: Test
ax_3 = (isAx
  (Sequent
    (IStructure
      (Positive (Atomic "x")))
    (OStructure
      (Positive (Atomic "x")))),
  False,
  "Trivial Ax-3"
  )
{------------------------------------------------------------------------------}
-- Trivial Ax-4 - Simple, atom-to-non-atom Ax-axiom
-- x+ |- [(x+ (tensor) x+)+]
ax_4 :: Test
ax_4 = (isAx
  (Sequent
    (IStructure
      (Positive (Atomic "x")))
    (OStructureF
      (Positive (Tensor
        (Positive (Atomic "x"))
        (Positive (Atomic "x")))))),
  False,
  "Trivial Ax-4"
  )
{------------------------------------------------------------------------------}
-- Trivial CoAx-1 - Trivial, correct, atomic CoAx-axiom
-- [x-] |- x-
coAx_1 :: Test
coAx_1 = (isCoAx
  (Sequent
    (IStructureF
      (Negative (Atomic "x")))
    (OStructure
      (Negative (Atomic "x")))),
  True,
  "Trivial CoAx-1"
  )
{------------------------------------------------------------------------------}
-- Trivial CoAx-2 - Simple, non-matching atomic Ax-axiom
-- [x-] |- y-
coAx_2 :: Test
coAx_2 = (isCoAx
  (Sequent
    (IStructureF
      (Negative (Atomic "x")))
    (OStructure
      (Negative (Atomic "y")))),
  False,
  "Trivial CoAx-2"
  )
{------------------------------------------------------------------------------}
-- Trivial CoAx-3 - Simple, non-focused, matching Ax-axiom
coAx_3 :: Test
coAx_3 = (isCoAx
  (Sequent
    (IStructure
      (Negative (Atomic "x")))
    (OStructure
      (Negative (Atomic "x")))),
  False,
  "Trivial CoAx-3"
  )
{------------------------------------------------------------------------------}
-- Trivial CoAx-4 - Simple, atom-to-non-atom Ax-axiom
coAx_4 :: Test
coAx_4 = (isCoAx
  (Sequent
    (IStructureF
      (Negative (Atomic "x")))
    (OStructure
      (Positive (Tensor
        (Negative (Atomic "x"))
        (Negative (Atomic "x")))))),
  False,
  "Trivial CoAx-4"
  )
{------------------------------------------------------------------------------}
-- Ax test list
axTests :: [Test]
axTests = [ax_1,ax_2,ax_3,ax_4,coAx_1,coAx_2,coAx_3,coAx_4]
