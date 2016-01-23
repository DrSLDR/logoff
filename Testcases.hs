{-
-- LoGoff testcases and testing functions
-- Actual unit testing frameworks are for scrubs
-}

import Logoff
import Datatypes
import TAP
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
tests = srTests ++ axTests ++ focusTests ++ monoTests ++ resTests

{------------------------------------------------------------------------------}
-- Show/Read block
{------------------------------------------------------------------------------}
-- NiceShow-1 - Neutral sequent output
niceShow_1 :: Test
niceShow_1 =((==)
  (niceShow
    (Sequent
      (IStruct (P (Positive "x")))
      (OStruct (P (Positive "x")))))
  "x+ |- x+",
  True,
  "NiceShow-1")
{------------------------------------------------------------------------------}
-- NiceShow-2 - Left-focused sequent output
niceShow_2 :: Test
niceShow_2 =((==)
  (niceShow
    (Sequent
      (FIStruct (P (Positive "x")))
      (OStruct (P (Positive "x")))))
  "[x+] |- x+",
  True,
  "NiceShow-2")
{------------------------------------------------------------------------------}
-- NiceShow-3 - Right-focused sequent output
niceShow_3 :: Test
niceShow_3 =((==)
  (niceShow
    (Sequent
      (IStruct (P (Positive "x")))
      (FOStruct (P (Positive "x")))))
  "x+ |- [x+]",
  True,
  "NiceShow-3")
{------------------------------------------------------------------------------}
-- NiceShow-4 - Tensors
niceShow_4 :: Test
niceShow_4 =((==)
  (niceShow
    (Sequent
      (STensor
        (IStruct (P (Positive "x")))
        (IStruct (P (Positive "y"))))
      (FOStruct
        (P (Tensor
          (P (Positive "x"))
          (P (Positive "y")))))))
  "(x+ .(x). y+) |- [(x+ (x) y+)]",
  True,
  "NiceShow-4")
{------------------------------------------------------------------------------}
-- NiceShow-5 - Sums
niceShow_5 :: Test
niceShow_5 =((==)
  (niceShow
    (Sequent
      (FIStruct
        (N (Sum
          (N (Negative "x"))
          (N (Negative "y")))))
      (SSum
        (OStruct (N (Negative "x")))
        (OStruct (N (Negative "y"))))))
  "[(x- (+) y-)] |- (x- .(+). y-)",
  True,
  "NiceShow-5")
{------------------------------------------------------------------------------}
-- NiceShow-6 - Left division
niceShow_6 :: Test
niceShow_6 =((==)
  (niceShow
    (Sequent
      (FIStruct
        (N (LDiv
          (P (Positive "x"))
          (N (Negative "y")))))
      (SLDiv
        (IStruct (P (Positive "x")))
        (OStruct (N (Negative "y"))))))
  "[(x+ \\ y-)] |- (x+ .\\. y-)",
  True,
  "NiceShow-6")
{------------------------------------------------------------------------------}
-- NiceShow-7 - Right division
niceShow_7 :: Test
niceShow_7 =((==)
  (niceShow
    (Sequent
      (FIStruct
        (N (RDiv
          (N (Negative "y"))
          (P (Positive "x")))))
      (SRDiv
        (OStruct (N (Negative "y")))
        (IStruct (P (Positive "x"))))))
  "[(y- / x+)] |- (y- ./. x+)",
  True,
  "NiceShow-7")
{------------------------------------------------------------------------------}
-- NiceShow-8 - Left difference
niceShow_8 :: Test
niceShow_8 =((==)
  (niceShow
    (Sequent
      (SLDiff
        (OStruct (N (Negative "y")))
        (IStruct (P (Positive "x"))))
      (FOStruct
        (P (LDiff
          (N (Negative "y"))
          (P (Positive "x")))))))
  "(y- .(\\). x+) |- [(y- (\\) x+)]",
  True,
  "NiceShow-8")
{------------------------------------------------------------------------------}
-- NiceShow-9 - Right difference
niceShow_9 :: Test
niceShow_9 =((==)
  (niceShow
    (Sequent
      (SRDiff
        (IStruct (P (Positive "x")))
        (OStruct (N (Negative "y"))))
      (FOStruct
        (P (RDiff
          (P (Positive "x"))
          (N (Negative "y")))))))
  "(x+ .(/). y-) |- [(x+ (/) y-)]",
  True,
  "NiceShow-9")
{------------------------------------------------------------------------------}
-- NiceRead-1 - Neutral sequent output
niceRead_1 :: Test
niceRead_1 =((==)
  (niceRead "x+ |- x+")
  (Sequent
    (IStruct (P (Positive "x")))
    (OStruct (P (Positive "x")))),
  True,
  "NiceRead-1")
{------------------------------------------------------------------------------}
-- NiceRead-2 - Left-focused sequent output
niceRead_2 :: Test
niceRead_2 =((==)
  (niceRead "[x+] |- x+")
  (Sequent
    (FIStruct (P (Positive "x")))
    (OStruct (P (Positive "x")))),
  True,
  "NiceRead-2")
{------------------------------------------------------------------------------}
-- NiceRead-3 - Right-focused sequent output
niceRead_3 :: Test
niceRead_3 =((==)
  (niceRead "x+ |- [x+]")
  (Sequent
    (IStruct (P (Positive "x")))
    (FOStruct (P (Positive "x")))),
  True,
  "NiceRead-3")
{------------------------------------------------------------------------------}
-- NiceRead-4 - Tensors
niceRead_4 :: Test
niceRead_4 =((==)
  (niceRead "(x+ .(x). y+) |- [(x+ (x) y+)]")
  (Sequent
    (STensor
      (IStruct (P (Positive "x")))
      (IStruct (P (Positive "y"))))
    (FOStruct
      (P (Tensor
        (P (Positive "x"))
        (P (Positive "y")))))),
  True,
  "NiceRead-4")
{------------------------------------------------------------------------------}
-- NiceRead-5 - Sums
niceRead_5 :: Test
niceRead_5 =((==)
  (niceRead "[(x- (+) y-)] |- (x- .(+). y-)")
  (Sequent
    (FIStruct
      (N (Sum
        (N (Negative "x"))
        (N (Negative "y")))))
    (SSum
      (OStruct (N (Negative "x")))
      (OStruct (N (Negative "y"))))),
  True,
  "NiceRead-5")
{------------------------------------------------------------------------------}
-- NiceRead-6 - Left division
niceRead_6 :: Test
niceRead_6 =((==)
  (niceRead "[(x+ \\ y-)] |- (x+ .\\. y-)")
  (Sequent
    (FIStruct
      (N (LDiv
        (P (Positive "x"))
        (N (Negative "y")))))
    (SLDiv
      (IStruct (P (Positive "x")))
      (OStruct (N (Negative "y"))))),
  True,
  "NiceRead-6")
{------------------------------------------------------------------------------}
-- NiceRead-7 - Right division
niceRead_7 :: Test
niceRead_7 =((==)
  (niceRead "[(y- / x+)] |- (y- ./. x+)")
  (Sequent
    (FIStruct
      (N (RDiv
        (N (Negative "y"))
        (P (Positive "x")))))
    (SRDiv
      (OStruct (N (Negative "y")))
      (IStruct (P (Positive "x"))))),
  True,
  "NiceRead-7")
{------------------------------------------------------------------------------}
-- NiceRead-8 - Left difference
niceRead_8 :: Test
niceRead_8 =((==)
  (niceRead "(y- .(\\). x+) |- [(y- (\\) x+)]")
  (Sequent
    (SLDiff
      (OStruct (N (Negative "y")))
      (IStruct (P (Positive "x"))))
    (FOStruct
      (P (LDiff
        (N (Negative "y"))
        (P (Positive "x")))))),
  True,
  "NiceRead-8")
{------------------------------------------------------------------------------}
-- NiceRead-9 - Right difference
niceRead_9 :: Test
niceRead_9 =((==)
  (niceRead "(x+ .(/). y-) |- [(x+ (/) y-)]")
  (Sequent
    (SRDiff
      (IStruct (P (Positive "x")))
      (OStruct (N (Negative "y"))))
    (FOStruct
      (P (RDiff
        (P (Positive "x"))
        (N (Negative "y")))))),
  True,
  "NiceRead-9")
{------------------------------------------------------------------------------}
-- Bi-directional-1 - Left-focused sequent
bidirectional_1 :: Test
bidirectional_1 =(let s = "x+ |- x+"
                      i = niceRead s :: Sequent
                      o = niceShow i in
  o == s,
  True,
  "Bi-directional-1")
{------------------------------------------------------------------------------}
-- Bi-directional-2 - Left-focused sequent
bidirectional_2 :: Test
bidirectional_2 =(let s = "[x+] |- x+"
                      i = niceRead s :: Sequent
                      o = niceShow i in
  o == s,
  True,
  "Bi-directional-2")
{------------------------------------------------------------------------------}
-- Bi-directional-3 - Right-focused sequent
bidirectional_3 :: Test
bidirectional_3 =(let s = "x+ |- [x+]"
                      i = niceRead s :: Sequent
                      o = niceShow i in
  o == s,
  True,
  "Bi-directional-3")
{------------------------------------------------------------------------------}
-- Bi-directional-4 - Tensors
bidirectional_4 :: Test
bidirectional_4 =(let s = "(x+ .(x). y+) |- [(x+ (x) y+)]"
                      i = niceRead s :: Sequent
                      o = niceShow i in
  o == s,
  True,
  "Bi-directional-4")
{------------------------------------------------------------------------------}
-- Bi-directional-5 - Sums
bidirectional_5 :: Test
bidirectional_5 =(let s = "[(x- (+) y-)] |- (x- .(+). y-)"
                      i = niceRead s :: Sequent
                      o = niceShow i in
  o == s,
  True,
  "Bi-directional-5")
{------------------------------------------------------------------------------}
-- Bi-directional-6 - Left division
bidirectional_6 :: Test
bidirectional_6 =(let s = "[(x+ \\ y-)] |- (x+ .\\. y-)"
                      i = niceRead s :: Sequent
                      o = niceShow i in
  o == s,
  True,
  "Bi-directional-6")
{------------------------------------------------------------------------------}
-- Bi-directional-7 - Right division
bidirectional_7 :: Test
bidirectional_7 =(let s = "[(y- / x+)] |- (y- ./. x+)"
                      i = niceRead s :: Sequent
                      o = niceShow i in
  o == s,
  True,
  "Bi-directional-7")
{------------------------------------------------------------------------------}
-- Bi-directional-8 - Left difference
bidirectional_8 :: Test
bidirectional_8 =(let s = "(y- .(\\). x+) |- [(y- (\\) x+)]"
                      i = niceRead s :: Sequent
                      o = niceShow i in
  o == s,
  True,
  "Bi-directional-8")
{------------------------------------------------------------------------------}
-- Bi-directional-9 - Right difference
bidirectional_9 :: Test
bidirectional_9 =(let s = "(x+ .(/). y-) |- [(x+ (/) y-)]"
                      i = niceRead s :: Sequent
                      o = niceShow i in
  o == s,
  True,
  "Bi-directional-9")
{------------------------------------------------------------------------------}
-- Show/Read test list
srTests :: [Test]
srTests = [niceShow_1, niceShow_2, niceShow_3, niceShow_4, niceShow_5,
  niceShow_6, niceShow_7, niceShow_8, niceShow_9, niceRead_1, niceRead_2,
  niceRead_3, niceRead_4, niceRead_5, niceRead_6, niceRead_7, niceRead_8,
  niceRead_9, bidirectional_1, bidirectional_2, bidirectional_3,
  bidirectional_4, bidirectional_5, bidirectional_6, bidirectional_7,
  bidirectional_8, bidirectional_9]

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
    (FOStruct
      (P (Positive "x")))),
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
    (FOStruct
      (P (Positive "y")))),
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
    (FOStruct
      (P (Tensor
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
    (FIStruct
      (N (Negative "x")))
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
    (FIStruct
      (N (Negative "x")))
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
    (FIStruct
      (N (Sum
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
      (FOStruct
        (P (Positive "x")))))
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
      (FIStruct
        (N (Negative "x")))
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
    (FOStruct
      (N (Negative "x")))),
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
    (FIStruct
      (P (Positive "x")))
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
-- x+ |- [x+]; y+ |- [y+] => (x+ .(x). y+) |- [(x+ (x) y+)]
monoTensor_1 :: Test
monoTensor_1 = ((==)
  (monoTensor
    (Sequent
      (IStruct (P (Positive "x")))
      (FOStruct (P (Positive "x"))))
    (Sequent
      (IStruct (P (Positive "y")))
      (FOStruct (P (Positive "y")))))
  (Sequent
    (STensor
      (IStruct (P (Positive "x")))
      (IStruct (P (Positive "y"))))
    (FOStruct
      (P (Tensor
        (P (Positive "x"))
        (P (Positive "y")))))),
  True,
  "monoTensor-1"
  )
{------------------------------------------------------------------------------}
-- monoSum-1 - Simple, axiomatic test
-- [x-] |- x-; [y-] |- y- => [(x- (+) y-)] |- (x- .(+). y-)
monoSum_1 :: Test
monoSum_1 = ((==)
  (monoSum
    (Sequent
      (FIStruct (N (Negative "x")))
      (OStruct (N (Negative "x"))))
    (Sequent
      (FIStruct (N (Negative "y")))
      (OStruct (N (Negative "y")))))
  (Sequent
    (FIStruct
      (N (Sum
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
-- x+ |- [x+]; [y-] |- y- => [(x+ \ y-)] |- (x+ .\. y-)
monoLDiv_1 :: Test
monoLDiv_1 = ((==)
  (monoLDiv
    (Sequent
      (IStruct (P (Positive "x")))
      (FOStruct (P (Positive "x"))))
    (Sequent
      (FIStruct (N (Negative "y")))
      (OStruct (N (Negative "y")))))
    (Sequent
      (FIStruct
        (N (LDiv
          (P (Positive "x"))
          (N (Negative "y")))))
      (SLDiv
        (IStruct (P (Positive "x")))
        (OStruct (N (Negative "y"))))),
  True,
  "monoLDiv-1")
{------------------------------------------------------------------------------}
-- monoRDiv-1 - Simple, axiomatic test
-- x+ |- [x+]; [y-] |- y- => [(y- / x+)] |- (y- ./. x+)
monoRDiv_1 :: Test
monoRDiv_1 = ((==)
  (monoRDiv
    (Sequent
      (IStruct (P (Positive "x")))
      (FOStruct (P (Positive "x"))))
    (Sequent
      (FIStruct (N (Negative "y")))
      (OStruct (N (Negative "y")))))
  (Sequent
    (FIStruct
      (N (RDiv
        (N (Negative "y"))
        (P (Positive "x")))))
    (SRDiv
      (OStruct (N (Negative "y")))
      (IStruct (P (Positive "x"))))),
  True,
  "monoRDiv-1")
{------------------------------------------------------------------------------}
-- monoLDiff-1 - Simple, axiomatic test
-- x+ |- [x+]; [y-] |- y- => (y- .(\). x+) |- [(y- (\) x+)]
monoLDiff_1 :: Test
monoLDiff_1 = ((==)
  (monoLDiff
    (Sequent
      (IStruct (P (Positive "x")))
      (FOStruct (P (Positive "x"))))
    (Sequent
      (FIStruct (N (Negative "y")))
      (OStruct (N (Negative "y")))))
  (Sequent
    (SLDiff
      (OStruct (N (Negative "y")))
      (IStruct (P (Positive "x"))))
    (FOStruct
      (P (LDiff
        (N (Negative "y"))
        (P (Positive "x")))))),
  True,
  "monoLDiff-1")
{------------------------------------------------------------------------------}
-- monoRDiff-1 - Simple, axiomatic test
-- x+ |- [x+]; [y-] |- y- => (x+ .(/). y-) |- [(x+ (/) y-)]
monoRDiff_1 :: Test
monoRDiff_1 = ((==)
  (monoRDiff
    (Sequent
      (IStruct (P (Positive "x")))
      (FOStruct (P (Positive "x"))))
    (Sequent
      (FIStruct (N (Negative "y")))
      (OStruct (N (Negative "y")))))
  (Sequent
    (SRDiff
      (IStruct (P (Positive "x")))
      (OStruct (N (Negative "y"))))
    (FOStruct
      (P (RDiff
        (P (Positive "x"))
        (N (Negative "y")))))),
  True,
  "monoRDiff-1")
{------------------------------------------------------------------------------}
-- Monotonicity test list
monoTests :: [Test]
monoTests = [monoTensor_1, monoSum_1, monoLDiv_1, monoRDiv_1, monoLDiff_1,
  monoRDiff_1]

{------------------------------------------------------------------------------}
-- Residuation block
{------------------------------------------------------------------------------}
-- res1_1 -- First step, residuation 1
-- x+ |- (z- ./. y+) => (x+ .(x). y+) |- z-
res1_1 :: Test
res1_1 = ((==)
  (residuate1
    (Sequent
      (IStruct (P (Positive "x")))
      (SRDiv
        (OStruct (N (Negative "z")))
        (IStruct (P (Positive "y"))))))
  (Sequent
    (STensor
      (IStruct (P (Positive "x")))
      (IStruct (P (Positive "y"))))
    (OStruct (N (Negative "z")))),
  True,
  "res1-1")
{------------------------------------------------------------------------------}
-- res1_2 -- Second step, residuation 1
-- (x+ .(x). y+) |- z- => y+ |- (x+ .\\. z-)
res1_2 :: Test
res1_2 = ((==)
  (residuate1
    (Sequent
      (STensor
        (IStruct (P (Positive "x")))
        (IStruct (P (Positive "y"))))
      (OStruct (N (Negative "z")))))
  (Sequent
    (IStruct (P (Positive "y")))
    (SLDiv
      (IStruct (P (Positive "x")))
      (OStruct (N (Negative "z"))))),
  True,
  "res1-2")
{------------------------------------------------------------------------------}
-- res1_3 -- Double step, residuation 1
-- x+ |- (z- ./. y+) => y+ |- (x+ .\\. z-)
res1_3 :: Test
res1_3 = ((==)
  ((residuate1 . residuate1)
    (Sequent
      (IStruct (P (Positive "x")))
      (SRDiv
        (OStruct (N (Negative "z")))
        (IStruct (P (Positive "y"))))))
  (Sequent
    (IStruct (P (Positive "y")))
    (SLDiv
      (IStruct (P (Positive "x")))
      (OStruct (N (Negative "z"))))),
  True,
  "res1-3")
{------------------------------------------------------------------------------}
-- res1i_1 -- First step, inverse residuation 1
-- y+ |- (x+ .\\. z-) => (x+ .(x). y+) |- z-
res1i_1 :: Test
res1i_1 = ((==)
  (residuate1i
    (Sequent
      (IStruct (P (Positive "y")))
      (SLDiv
        (IStruct (P (Positive "x")))
        (OStruct (N (Negative "z"))))))
  (Sequent
    (STensor
      (IStruct (P (Positive "x")))
      (IStruct (P (Positive "y"))))
    (OStruct (N (Negative "z")))),
  True,
  "res1i-1")
{------------------------------------------------------------------------------}
-- res1i_2 -- Second step, inverse residuation 1
-- (x+ .(x). y+) |- z- => x+ |- (z- ./. y+)
res1i_2 :: Test
res1i_2 = ((==)
  (residuate1i
    (Sequent
      (STensor
        (IStruct (P (Positive "x")))
        (IStruct (P (Positive "y"))))
      (OStruct (N (Negative "z")))))
  (Sequent
    (IStruct (P (Positive "x")))
    (SRDiv
      (OStruct (N (Negative "z")))
      (IStruct (P (Positive "y"))))),
  True,
  "res1i-2")
{------------------------------------------------------------------------------}
-- res1i_3 -- Double step, inverse residuation 1
-- y+ |- (x+ .\\. z-) => x+ |- (z- ./. y+)
res1i_3 :: Test
res1i_3 = ((==)
  ((residuate1i . residuate1i)
    (Sequent
      (IStruct (P (Positive "y")))
      (SLDiv
        (IStruct (P (Positive "x")))
        (OStruct (N (Negative "z"))))))
  (Sequent
    (IStruct (P (Positive "x")))
    (SRDiv
      (OStruct (N (Negative "z")))
      (IStruct (P (Positive "y"))))),
  True,
  "res1i-3")
{------------------------------------------------------------------------------}
-- res2_1 -- First step, residuation 2
-- (y- .(\\). z-) |- x+ => z- |- (y- .(+). x+)
res2_1 :: Test
res2_1 = ((==)
  (residuate2
    (Sequent
      (SLDiff
        (OStruct (N (Negative "y")))
        (IStruct (N (Negative "z"))))
      (OStruct (P (Positive "x")))))
  (Sequent
    (IStruct (N (Negative "z")))
    (SSum
      (OStruct (N (Negative "y")))
      (OStruct (P (Positive "x"))))),
  True,
  "res2-1")
{------------------------------------------------------------------------------}
-- res2_2 -- Second step, residuation 2
-- z- |- (y- .(+). x+) => (z- .(/). x+) |- y-
res2_2 :: Test
res2_2 = ((==)
  (residuate2
    (Sequent
      (IStruct (N (Negative "z")))
      (SSum
        (OStruct (N (Negative "y")))
        (OStruct (P (Positive "x"))))))
  (Sequent
    (SRDiff
      (IStruct (N (Negative "z")))
      (OStruct (P (Positive "x"))))
    (OStruct (N (Negative "y")))),
  True,
  "res2-2")
{------------------------------------------------------------------------------}
-- res2_3 -- Double step, residuation 2
-- (y- .(\\). z-) |- x+ => (z- .(/). x+) |- y-
res2_3 :: Test
res2_3 = ((==)
  ((residuate2 . residuate2)
    (Sequent
      (SLDiff
        (OStruct (N (Negative "y")))
        (IStruct (N (Negative "z"))))
      (OStruct (P (Positive "x")))))
    (Sequent
      (SRDiff
        (IStruct (N (Negative "z")))
        (OStruct (P (Positive "x"))))
      (OStruct (N (Negative "y")))),
  True,
  "res2-3")
{------------------------------------------------------------------------------}
-- res2i_1 -- First step, inverse residuation 2
-- (z- .(/). x+) |- y- => z- |- (y- .(+). x+)
res2i_1 :: Test
res2i_1 = ((==)
  (residuate2i
    (Sequent
      (SRDiff
        (IStruct (N (Negative "z")))
        (OStruct (P (Positive "x"))))
      (OStruct (N (Negative "y")))))
  (Sequent
    (IStruct (N (Negative "z")))
    (SSum
      (OStruct (N (Negative "y")))
      (OStruct (P (Positive "x"))))),
  True,
  "res2i-1")
{------------------------------------------------------------------------------}
-- res2i_2 -- Second step, inverse residuation 2
-- z- |- (y- .(+). x+) => (y- .(\\). z-) |- x+
res2i_2 :: Test
res2i_2 = ((==)
  (residuate2i
    (Sequent
      (IStruct (N (Negative "z")))
      (SSum
        (OStruct (N (Negative "y")))
        (OStruct (P (Positive "x"))))))
  (Sequent
    (SLDiff
      (OStruct (N (Negative "y")))
      (IStruct (N (Negative "z"))))
    (OStruct (P (Positive "x")))),
  True,
  "res2i-2")
{------------------------------------------------------------------------------}
-- res2i_3 -- Double step, inverse residuation 2
-- (z- .(/). x+) |- y- => (y- .(\\). z-) |- x+
res2i_3 :: Test
res2i_3 = ((==)
  ((residuate2i . residuate2i)
    (Sequent
      (SRDiff
        (IStruct (N (Negative "z")))
        (OStruct (P (Positive "x"))))
      (OStruct (N (Negative "y")))))
  (Sequent
    (SLDiff
      (OStruct (N (Negative "y")))
      (IStruct (N (Negative "z"))))
    (OStruct (P (Positive "x")))),
  True,
  "res2i-3")
{------------------------------------------------------------------------------}
-- Residuation test list
resTests :: [Test]
resTests = [res1_1, res1_2, res1_3, res1i_1, res1i_2, res1i_3, res2_1, res2_2,
  res2_3, res2i_1, res2i_2, res2i_3]
