{-
-- Residuation block
-}

module TestsRes
    (
    resTests
    ) where

import Logoff
import Datatypes

{------------------------------------------------------------------------------}
-- res1_1 -- First step, residuation 1
-- x+ |- (z- ./. y+) => (x+ .(x). y+) |- z-
res1_1 :: Test
res1_1 = ((==)
  (residuate1 1
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
  (residuate1 2
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
  ((residuate1 2 . residuate1 1)
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
  (residuate1i 2
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
  (residuate1i 1
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
  ((residuate1i 1 . residuate1i 2)
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
  (residuate2 1
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
  (residuate2 2
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
  ((residuate2 2 . residuate2 1)
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
  (residuate2i 2
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
  (residuate2i 1
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
  ((residuate2i 1 . residuate2i 2)
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
