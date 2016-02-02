{-
-- Show/Read block
-}

module TestsShowRead
      (
      srTests
      ) where

import Logoff
import Datatypes
import TAP

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
