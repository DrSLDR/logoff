{-
-- Axiom test block
-}

module TestsAxiom
      (
      axTests
      ) where

import Logoff
import Datatypes

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
