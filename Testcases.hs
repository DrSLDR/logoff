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
tests = axTests ++ focusTests ++ monoTests

{------------------------------------------------------------------------------}
-- Axiom block
{------------------------------------------------------------------------------}
-- Trivial Ax-1 - Trivial, correct, atomic Ax-axiom
-- x+ |- [x+]
ax_1 :: Test
ax_1 = (isAx
  (Sequent
    (IStruct
      (P (Positive "x")))
    (OStruct
      (FP (Positive "x")))),
  True,
  "Trivial Ax-1"
  )
{------------------------------------------------------------------------------}
-- Trivial Ax-2 - Simple, non-matching atomic Ax-axiom
-- x+ |- [y+]
ax_2 :: Test
ax_2 = (isAx
  (Sequent
    (IStruct
      (P (Positive "x")))
    (OStruct
      (FP (Positive "y")))),
  False,
  "Trivial Ax-2"
  )
{------------------------------------------------------------------------------}
-- Trivial Ax-3 - Simple, non-focused, matching Ax-axiom
-- x+ |- x+
ax_3 :: Test
ax_3 = (isAx
  (Sequent
    (IStruct
      (P (Positive "x")))
    (OStruct
      (P (Positive "x")))),
  False,
  "Trivial Ax-3"
  )
{------------------------------------------------------------------------------}
-- Trivial Ax-4 - Simple, atom-to-non-atom Ax-axiom
-- x+ |- [(x+ (tensor) x+)+]
ax_4 :: Test
ax_4 = (isAx
  (Sequent
    (IStruct
      (P (Positive "x")))
    (OStruct
      (FP (Tensor
        (P (Positive "x"))
        (P (Positive "x")))))),
  False,
  "Trivial Ax-4"
  )
{------------------------------------------------------------------------------}
-- Trivial CoAx-1 - Trivial, correct, atomic CoAx-axiom
-- [x-] |- x-
coAx_1 :: Test
coAx_1 = (isCoAx
  (Sequent
    (IStruct
      (FN (Negative "x")))
    (OStruct
      (N (Negative "x")))),
  True,
  "Trivial CoAx-1"
  )
{------------------------------------------------------------------------------}
-- Trivial CoAx-2 - Simple, non-matching atomic Ax-axiom
-- [x-] |- y-
coAx_2 :: Test
coAx_2 = (isCoAx
  (Sequent
    (IStruct
      (FN (Negative "x")))
    (OStruct
      (N (Negative "y")))),
  False,
  "Trivial CoAx-2"
  )
{------------------------------------------------------------------------------}
-- Trivial CoAx-3 - Simple, non-focused, matching Ax-axiom
-- x- |- x-
coAx_3 :: Test
coAx_3 = (isCoAx
  (Sequent
    (IStruct
      (N (Negative "x")))
    (OStruct
      (N (Negative "x")))),
  False,
  "Trivial CoAx-3"
  )
{------------------------------------------------------------------------------}
-- Trivial CoAx-4 - Simple, atom-to-non-atom Ax-axiom
-- [(x- (sum) x-)-] |- x-
coAx_4 :: Test
coAx_4 = (isCoAx
  (Sequent
    (IStruct
      (FN (Sum
        (N (Negative "x"))
        (N (Negative "x")))))
    (OStruct
      (N (Negative "x")))),
  False,
  "Trivial CoAx-4"
  )
{------------------------------------------------------------------------------}
-- Ax test list
axTests :: [Test]
axTests = [ax_1,ax_2,ax_3,ax_4,coAx_1,coAx_2,coAx_3,coAx_4]

{------------------------------------------------------------------------------}
-- Focusing block
{------------------------------------------------------------------------------}
-- Defocus R-1 - Simple defocusing action
-- x+ |- [x+] => x+ |- x+
defocusR_1 :: Test
defocusR_1 = ((==)
  (defocusR
    (Sequent
      (IStruct
        (P (Positive "x")))
      (OStruct
        (FP (Positive "x")))))
    (Sequent
      (IStruct
        (P (Positive "x")))
      (OStruct
        (P (Positive "x")))),
  True,
  "Defocus R-1"
  )
{------------------------------------------------------------------------------}
-- Defocus L-1 - Simple defocusing action
-- [x-] |- x- => x- |- x-
defocusL_1 :: Test
defocusL_1 = ((==)
  (defocusL
    (Sequent
      (IStruct
        (FN (Negative "x")))
      (OStruct
        (N (Negative "x")))))
    (Sequent
      (IStruct
        (N (Negative "x")))
      (OStruct
        (N (Negative"x")))),
  True,
  "Defocus L-1"
  )
{------------------------------------------------------------------------------}
-- Focus R-1 - Simple focusing action
-- x- |- x- => x- |- [x-]
focusR_1 :: Test
focusR_1 = ((==)
  (focusR
    (Sequent
      (IStruct
        (N (Negative "x")))
      (OStruct
        (N (Negative "x")))))
  (Sequent
    (IStruct
      (N (Negative "x")))
    (OStruct
      (FN (Negative "x")))),
  True,
  "Focus R-1"
  )
{------------------------------------------------------------------------------}
-- Focus L-1 - Simple focusing action
-- x+ |- x+ => [x+] |- x+
focusL_1 :: Test
focusL_1 = ((==)
  (focusL
    (Sequent
      (IStruct
        (P (Positive "x")))
      (OStruct
        (P (Positive"x")))))
  (Sequent
    (IStruct
      (FP (Positive "x")))
    (OStruct
      (P (Positive "x")))),
  True,
  "Focus L-1"
  )
{------------------------------------------------------------------------------}
-- Focus test list
focusTests :: [Test]
focusTests = [defocusR_1, defocusL_1,focusR_1,focusL_1]

{------------------------------------------------------------------------------}
-- Monotonicity block
{------------------------------------------------------------------------------}
-- monoTensor-1 - Simple, axiomatic test
-- x+ |- [x+]; y+ |- [y+] => x+ STensor y+ |- [(x+ tensor y+)+]
monoTensor_1 :: Test
monoTensor_1 = ((==)
  (monoTensor
    (Sequent
      (IStruct (P (Positive "x")))
      (OStruct (FP (Positive "x"))))
    (Sequent
      (IStruct (P (Positive "y")))
      (OStruct (FP (Positive "y")))))
  (Sequent
    (STensor
      (IStruct (P (Positive "x")))
      (IStruct (P (Positive "y"))))
    (OStruct
      (FP (Tensor
        (P (Positive "x"))
        (P (Positive "y")))))),
  True,
  "monoTensor-1"
  )
{------------------------------------------------------------------------------}
-- monoSum-1 - Simple, axiomatic test
-- [x-] |- x-; [y-] |- y- => [(x- + y-)-] |- x- SSum y-
monoSum_1 :: Test
monoSum_1 = ((==)
  (monoSum
    (Sequent
      (IStruct (FN (Negative "x")))
      (OStruct (N (Negative "x"))))
    (Sequent
      (IStruct (FN (Negative "y")))
      (OStruct (N (Negative "y")))))
  (Sequent
    (IStruct
      (FN (Sum
        (N (Negative "x"))
        (N (Negative "y")))))
    (SSum
      (OStruct (N (Negative "x")))
      (OStruct (N (Negative "y"))))),
  True,
  "monoSum-1"
  )
{------------------------------------------------------------------------------}
-- monoLDiv-1 - Simple, axiomatic test
-- x+ |- [x+]; [y-] |- y- => [(x+ \ y-)-] |- x+ SLDiv y-
monoLDiv_1 :: Test
monoLDiv_1 = ((==)
  (monoLDiv
    (Sequent
      (IStruct (P (Positive "x")))
      (OStruct (FP (Positive "x"))))
    (Sequent
      (IStruct (FN (Negative "y")))
      (OStruct (N (Negative "y")))))
    (Sequent
      (IStruct
        (FN (LDiv
          (P (Positive "x"))
          (N (Negative "y")))))
      (SLDiv
        (IStruct (P (Positive "x")))
        (OStruct (N (Negative "y"))))),
  True,
  "monoLDiv-1")
{------------------------------------------------------------------------------}
-- monoRDiv-1 - Simple, axiomatic test
-- x+ |- [x+]; [y-] |- y- => [(y- / x+)-] |- y- SRDiv x+
monoRDiv_1 :: Test
monoRDiv_1 = ((==)
  (monoRDiv
    (Sequent
      (IStruct (P (Positive "x")))
      (OStruct (FP (Positive "x"))))
    (Sequent
      (IStruct (FN (Negative "y")))
      (OStruct (N (Negative "y")))))
  (Sequent
    (IStruct
      (FN (RDiv
        (N (Negative "y"))
        (P (Positive "x")))))
    (SRDiv
      (OStruct (N (Negative "y")))
      (IStruct (P (Positive "x"))))),
  True,
  "monoLDiv-1")
{------------------------------------------------------------------------------}
-- monoLDiff-1 - Simple, axiomatic test
-- x+ |- [x+]; [y-] |- y- => y- SLDiff x+ |- [(y- (\) x+)+]
monoLDiff_1 :: Test
monoLDiff_1 = ((==)
  (monoLDiff
    (Sequent
      (IStruct (P (Positive "x")))
      (OStruct (FP (Positive "x"))))
    (Sequent
      (IStruct (FN (Negative "y")))
      (OStruct (N (Negative "y")))))
  (Sequent
    (SLDiff
      (OStruct (N (Negative "y")))
      (IStruct (P (Positive "x"))))
    (OStruct
      (FP (LDiff
        (N (Negative "y"))
        (P (Positive "x")))))),
  True,
  "monoLDiv-1")
{------------------------------------------------------------------------------}
-- monoRDiff-1 - Simple, axiomatic test
-- x+ |- [x+]; [y-] |- y- => x+ SRDiff y- |- [(x+ (/) y-)+]
monoRDiff_1 :: Test
monoRDiff_1 = ((==)
  (monoRDiff
    (Sequent
      (IStruct (P (Positive "x")))
      (OStruct (FP (Positive "x"))))
    (Sequent
      (IStruct (FN (Negative "y")))
      (OStruct (N (Negative "y")))))
  (Sequent
    (SRDiff
      (IStruct (P (Positive "x")))
      (OStruct (N (Negative "y"))))
    (OStruct
      (FP (RDiff
        (P (Positive "x"))
        (N (Negative "y")))))),
  True,
  "monoLDiv-1")
{------------------------------------------------------------------------------}
-- Monotonicity test list
monoTests :: [Test]
monoTests = [monoTensor_1, monoSum_1, monoLDiv_1, monoRDiv_1, monoLDiff_1,
  monoRDiff_1]
