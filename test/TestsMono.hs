{-
-- (Inverse) Monotonicity block
-}

module TestsMono
    (
    monoTests
    ) where

import Logoff
import Datatypes

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

{- Inverse monotonicity block -------------------------------------------------}

-- iMonoTensor-1 - Simple, axiomatic test
-- (x+ .(x). y+) |- [(x+ (x) y+)] => x+ |- [x+]; y+ |- [y+]
iMonoTensor_1 :: Test
iMonoTensor_1 = ((==)
  (iMonoTensor
    (Sequent
      (STensor
        (IStruct (P (Positive "x")))
        (IStruct (P (Positive "y"))))
      (FOStruct
        (P (Tensor
          (P (Positive "x"))
          (P (Positive "y")))))))
  (Sequent
    (IStruct (P (Positive "x")))
    (FOStruct (P (Positive "x"))),
  Sequent
    (IStruct (P (Positive "y")))
    (FOStruct (P (Positive "y")))),
  True,
  "iMonoTensor-1"
  )
{------------------------------------------------------------------------------}
-- iMonoSum-1 - Simple, axiomatic test
-- [(x- (+) y-)] |- (x- .(+). y-) => [x-] |- x-; [y-] |- y-
iMonoSum_1 :: Test
iMonoSum_1 = ((==)
  (iMonoSum
    (Sequent
      (FIStruct
        (N (Sum
          (N (Negative "x"))
          (N (Negative "y")))))
      (SSum
        (OStruct (N (Negative "x")))
        (OStruct (N (Negative "y"))))))
  (Sequent
    (FIStruct (N (Negative "x")))
    (OStruct (N (Negative "x"))),
  Sequent
    (FIStruct (N (Negative "y")))
    (OStruct (N (Negative "y")))),
  True,
  "iMonoSum-1"
  )
{------------------------------------------------------------------------------}
-- iMonoLDiv-1 - Simple, axiomatic test
-- [(x+ \ y-)] |- (x+ .\. y-) => x+ |- [x+]; [y-] |- y-
iMonoLDiv_1 :: Test
iMonoLDiv_1 = ((==)
  (iMonoLDiv
    (Sequent
      (FIStruct
        (N (LDiv
          (P (Positive "x"))
          (N (Negative "y")))))
      (SLDiv
        (IStruct (P (Positive "x")))
        (OStruct (N (Negative "y"))))))
  (Sequent
    (IStruct (P (Positive "x")))
    (FOStruct (P (Positive "x"))),
  Sequent
    (FIStruct (N (Negative "y")))
    (OStruct (N (Negative "y")))),
  True,
  "iMonoLDiv-1")
{------------------------------------------------------------------------------}
-- iMonoRDiv-1 - Simple, axiomatic test
-- [(y- / x+)] |- (y- ./. x+) => x+ |- [x+]; [y-] |- y-
iMonoRDiv_1 :: Test
iMonoRDiv_1 = ((==)
  (iMonoRDiv
    (Sequent
      (FIStruct
        (N (RDiv
          (N (Negative "y"))
          (P (Positive "x")))))
      (SRDiv
        (OStruct (N (Negative "y")))
        (IStruct (P (Positive "x"))))))
  (Sequent
    (IStruct (P (Positive "x")))
    (FOStruct (P (Positive "x"))),
  Sequent
    (FIStruct (N (Negative "y")))
    (OStruct (N (Negative "y")))),
  True,
  "iMonoRDiv-1")
{------------------------------------------------------------------------------}
-- iMonoLDiff-1 - Simple, axiomatic test
-- (y- .(\). x+) |- [(y- (\) x+)] => x+ |- [x+]; [y-] |- y-
iMonoLDiff_1 :: Test
iMonoLDiff_1 = ((==)
  (iMonoLDiff
    (Sequent
      (SLDiff
        (OStruct (N (Negative "y")))
        (IStruct (P (Positive "x"))))
      (FOStruct
        (P (LDiff
          (N (Negative "y"))
          (P (Positive "x")))))))
  (Sequent
    (IStruct (P (Positive "x")))
    (FOStruct (P (Positive "x"))),
  Sequent
    (FIStruct (N (Negative "y")))
    (OStruct (N (Negative "y")))),
  True,
  "iMonoLDiff-1")
{------------------------------------------------------------------------------}
-- iMonoRDiff-1 - Simple, axiomatic test
-- (x+ .(/). y-) |- [(x+ (/) y-)] => x+ |- [x+]; [y-] |- y-
iMonoRDiff_1 :: Test
iMonoRDiff_1 = ((==)
  (iMonoRDiff
    (Sequent
      (SRDiff
        (IStruct (P (Positive "x")))
        (OStruct (N (Negative "y"))))
      (FOStruct
        (P (RDiff
          (P (Positive "x"))
          (N (Negative "y")))))))
  (Sequent
    (IStruct (P (Positive "x")))
    (FOStruct (P (Positive "x"))),
  Sequent
    (FIStruct (N (Negative "y")))
    (OStruct (N (Negative "y")))),
  True,
  "iMonoRDiff-1")
{------------------------------------------------------------------------------}
-- Monotonicity test list
monoTests :: [Test]
monoTests = [monoTensor_1, monoSum_1, monoLDiv_1, monoRDiv_1, monoLDiff_1,
  monoRDiff_1, iMonoTensor_1, iMonoSum_1, iMonoLDiv_1, iMonoRDiv_1,
  iMonoLDiff_1, iMonoRDiff_1]
