{-
-- Focusing block
-}

module TestsFocus
    (
    focusTests
    ) where

import Logoff
import Datatypes

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
-- Inverse defocus R-1 - Simple defocusing action
-- x+ |- x+ => x+ |- [x+]
idefocusR_1 :: Test
idefocusR_1 = ((==)
  (idefocusR
    (Sequent
      (IStruct
        (P (Positive "x")))
      (OStruct
        (P (Positive "x")))))
  (Sequent
    (IStruct
      (P (Positive "x")))
    (FOStruct
      (P (Positive "x")))),
  True,
  "Inverse defocus R-1"
  )
{------------------------------------------------------------------------------}
-- Inverse defocus L-1 - Simple defocusing action
-- x- |- x- => [x-] |- x-
idefocusL_1 :: Test
idefocusL_1 = ((==)
  (idefocusL
    (Sequent
      (IStruct
        (N (Negative "x")))
      (OStruct
        (N (Negative"x")))))
  (Sequent
    (FIStruct
      (N (Negative "x")))
    (OStruct
      (N (Negative "x")))),
  True,
  "Inverse defocus L-1"
  )
{------------------------------------------------------------------------------}
-- Inverse focus R-1 - Simple focusing action
-- x- |- [x-] => x- |- x-
ifocusR_1 :: Test
ifocusR_1 = ((==)
  (ifocusR
    (Sequent
      (IStruct
        (N (Negative "x")))
      (FOStruct
        (N (Negative "x")))))
  (Sequent
    (IStruct
      (N (Negative "x")))
    (OStruct
      (N (Negative "x")))),
  True,
  "Inverse focus R-1"
  )
{------------------------------------------------------------------------------}
-- Inverse focus L-1 - Simple focusing action
-- [x+] |- x+ => x+ |- x+
ifocusL_1 :: Test
ifocusL_1 = ((==)
  (ifocusL
    (Sequent
      (FIStruct
        (P (Positive "x")))
      (OStruct
        (P (Positive "x")))))
  (Sequent
    (IStruct
      (P (Positive "x")))
    (OStruct
      (P (Positive"x")))),
  True,
  "Inverse focus L-1"
  )
{------------------------------------------------------------------------------}
-- Focus test list
focusTests :: [Test]
focusTests = [defocusR_1, defocusL_1, focusR_1, focusL_1, idefocusR_1,
  idefocusL_1, ifocusR_1, ifocusL_1]
