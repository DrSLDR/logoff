{-
-- LoGoff system
-}

module Logoff where

import Datatypes
import Data.Maybe

{------------------------------------------------------------------------------}
-- Axiom block
{------------------------------------------------------------------------------}
-- isAx verifies if a sequent is an Ax-type axiom
isAx :: Sequent -> Bool
isAx (RFocus (IStruct (P (Positive i))) (P (Positive o))) =
  o == i
isAx _ = False

-- isCoAx verifies if a sequent is a CoAx-type axiom
isCoAx :: Sequent -> Bool
isCoAx (LFocus (N (Negative i)) (OStruct (N (Negative o)))) =
  o == i
isCoAx _ = False

{------------------------------------------------------------------------------}
-- Focusing block
-- Note that if the functions can't treat the given sequent, they return it
{------------------------------------------------------------------------------}
-- Defocus right (or top-down focus right)
defocusR :: Sequent -> Sequent
defocusR (RFocus i (P o)) = Neutral i (OStruct (P o))
defocusR s = s

-- Inverse defocus right (top-down focus right)
idefocusR :: Sequent -> Sequent
idefocusR (Neutral i (OStruct (P o))) = RFocus i (P o)
idefocusR s = s

-- Defocus left (or top-down focus left)
defocusL :: Sequent -> Sequent
defocusL (LFocus (N i) o) = Neutral (IStruct (N i)) o
defocusL s = s

-- Inverse defocus left (top-down focus left)
idefocusL :: Sequent -> Sequent
idefocusL (Neutral (IStruct (N i)) o) = LFocus (N i) o
idefocusL s = s

-- Focus right (or top-down defocus right)
focusR :: Sequent -> Sequent
focusR (Neutral i (OStruct (N o))) = RFocus i (N o)
focusR s = s

-- Inverse focus right (top-down defocus right)
ifocusR :: Sequent -> Sequent
ifocusR (RFocus i (N o)) = Neutral i (OStruct (N o))
ifocusR s = s

-- Focus left (or top-down defocus left)
focusL :: Sequent -> Sequent
focusL (Neutral (IStruct (P i)) o) = LFocus (P i) o
focusL s = s

-- Inverse focus left (top-down defocus left)
ifocusL :: Sequent -> Sequent
ifocusL (LFocus (P i) o) = Neutral (IStruct (P i)) o
ifocusL s = s

{------------------------------------------------------------------------------}
-- Monotonicity block
-- Note that if the functions can't treat the given sequent, they return the
-- left one
{------------------------------------------------------------------------------}
-- Tensor - introduces tensor
monoTensor :: Sequent -> Sequent -> Sequent
monoTensor (RFocus xi xo) (RFocus yi yo) =
  RFocus (STensor xi yi) (P (Tensor xo yo))
monoTensor l r = l

-- Sum - introduces sum
monoSum :: Sequent -> Sequent -> Sequent
monoSum (LFocus xi xo) (LFocus yi yo) = LFocus (N (Sum xi yi)) (SSum xo yo)
monoSum l r = l

-- Left division - introduces LDiv
monoLDiv :: Sequent -> Sequent -> Sequent
monoLDiv (RFocus xi xo) (LFocus yi yo) = LFocus (N (LDiv xo yi)) (SLDiv xi yo)
monoLDiv l r = l

-- Right division - introduces RDiv
monoRDiv :: Sequent -> Sequent -> Sequent
monoRDiv (LFocus yi yo) (RFocus xi xo) = LFocus (N (RDiv yi xo)) (SRDiv yo xi)
monoRDiv l r = l

-- Left difference - introduces LDiff
monoLDiff :: Sequent -> Sequent -> Sequent
monoLDiff (LFocus yi yo) (RFocus xi xo) =
  RFocus (SLDiff yo xi) (P (LDiff yi xo))
monoLDiff l r = l

-- Right difference - introduces RDiff
monoRDiff :: Sequent -> Sequent -> Sequent
monoRDiff (RFocus xi xo) (LFocus yi yo) =
  RFocus (SRDiff xi yo) (P (RDiff xo yi))
monoRDiff l r = l

{------------------------------------------------------------------------------}
-- Inverse monotonicity block
-- Note that if the functions can't treat the given sequent, they return it
{------------------------------------------------------------------------------}
-- Tensor - removes tensor
iMonoTensor :: Sequent -> (Sequent, Sequent)
iMonoTensor (RFocus (STensor xi yi) (P (Tensor xo yo))) =
  (RFocus xi xo, RFocus yi yo)
iMonoTensor s = (s,s)

-- Sum - removes sum
iMonoSum :: Sequent -> (Sequent, Sequent)
iMonoSum (LFocus (N (Sum xi yi)) (SSum xo yo)) =
  (LFocus xi xo, LFocus yi yo)
iMonoSum s = (s,s)

-- Left division - removes LDiv
iMonoLDiv :: Sequent -> (Sequent, Sequent)
iMonoLDiv (LFocus (N (LDiv xo yi)) (SLDiv xi yo)) =
  (RFocus xi xo, LFocus yi yo)
iMonoLDiv s = (s,s)

-- Right division - removes RDiv
iMonoRDiv :: Sequent -> (Sequent, Sequent)
iMonoRDiv (LFocus (N (RDiv yi xo)) (SRDiv yo xi)) =
  (LFocus yi yo, RFocus xi xo)
iMonoRDiv s = (s,s)

-- Left difference - removes LDiff
iMonoLDiff :: Sequent -> (Sequent, Sequent)
iMonoLDiff (RFocus (SLDiff yo xi) (P (LDiff yi xo))) =
  (LFocus yi yo, RFocus xi xo)
iMonoLDiff s = (s,s)

-- Right difference - removes RDiff
iMonoRDiff :: Sequent -> (Sequent, Sequent)
iMonoRDiff (RFocus (SRDiff xi yo) (P (RDiff xo yi))) =
  (RFocus xi  xo, LFocus yi yo)
iMonoRDiff s = (s,s)

{------------------------------------------------------------------------------}
-- Residuation block
-- Note that if the functions can't treat the given sequent, they return it
{------------------------------------------------------------------------------}
-- residuate1 - Downwards R1 rule
residuate1 :: Int -> Sequent -> Sequent
residuate1 1 (Neutral x (SRDiv z y)) = Neutral (STensor x y) z
residuate1 2 (Neutral (STensor x y) z) = Neutral y (SLDiv x z)
residuate1 _ s = s

-- residuate1i - Upwards (inverted) R1 rule
residuate1i :: Int -> Sequent -> Sequent
residuate1i 2 (Neutral y (SLDiv x z)) = Neutral (STensor x y) z
residuate1i 1 (Neutral (STensor x y) z) = Neutral x (SRDiv z y)
residuate1i _ s = s

-- residuate2 - Downwards R2 rule
residuate2 :: Int -> Sequent -> Sequent
residuate2 1 (Neutral (SLDiff y z) x) = Neutral z (SSum y x)
residuate2 2 (Neutral z (SSum y x)) = Neutral (SRDiff z x) y
residuate2 _ s = s

-- residuate2i - Upwards (inverted) R2 rule
residuate2i :: Int -> Sequent -> Sequent
residuate2i 2 (Neutral (SRDiff z x) y) = Neutral z (SSum y x)
residuate2i 1 (Neutral z (SSum y x)) = Neutral (SLDiff y z) x
residuate2i _ s = s

{------------------------------------------------------------------------------}
-- Rewrite block
-- Note that if the functions can't treat the given sequent, they return it
{------------------------------------------------------------------------------}
-- rewriteL - rewrites tensor, left and right difference (structure to logical)
rewriteL :: Sequent -> Sequent
rewriteL (Neutral (STensor (IStruct x) (IStruct y)) o) =
  Neutral (IStruct (P (Tensor x y))) o
rewriteL (Neutral (SRDiff (IStruct x) (OStruct y)) o) =
  Neutral (IStruct (P (RDiff x y))) o
rewriteL (Neutral (SLDiff (OStruct x) (IStruct y)) o) =
  Neutral (IStruct (P (LDiff x y))) o
rewriteL s = s

-- rewriteR - rewrites sum, left and right division (structure to logical)
rewriteR :: Sequent -> Sequent
rewriteR (Neutral i (SSum (OStruct x) (OStruct y))) =
  Neutral i (OStruct (N (Sum x y)))
rewriteR (Neutral i (SRDiv (OStruct x) (IStruct y))) =
  Neutral i (OStruct (N (RDiv x y)))
rewriteR (Neutral i (SLDiv (IStruct x) (OStruct y))) =
  Neutral i (OStruct (N (LDiv x y)))
rewriteR s = s

-- rewriteLi - rewrites tensor, left and right difference (logical to structure)
rewriteLi :: Sequent -> Sequent
rewriteLi (Neutral (IStruct (P (Tensor x y))) o) =
  Neutral (STensor (IStruct x) (IStruct y)) o
rewriteLi (Neutral (IStruct (P (RDiff x y))) o) =
  Neutral (SRDiff (IStruct x) (OStruct y)) o
rewriteLi (Neutral (IStruct (P (LDiff x y))) o) =
  Neutral (SLDiff (OStruct x) (IStruct y)) o
rewriteLi s = s

-- rewriteRi - rewrites sum, left and right division (logical to structure)
rewriteRi :: Sequent -> Sequent
rewriteRi (Neutral i (OStruct (N (Sum x y)))) =
  Neutral i (SSum (OStruct x) (OStruct y))
rewriteRi (Neutral i (OStruct (N (RDiv x y)))) =
  Neutral i (SRDiv (OStruct x) (IStruct y))
rewriteRi (Neutral i (OStruct (N (LDiv x y)))) =
  Neutral i (SLDiv (IStruct x) (OStruct y))
rewriteRi s = s

{------------------------------------------------------------------------------}
-- Top-Down solver block
{------------------------------------------------------------------------------}
-- tdSolve - Master Top-Down solver
-- Also, yay maybe monad
tdSolve :: Sequent -> Maybe ProofTree
tdSolve s
  | isAx s = Just (Ax s)
  | isCoAx s = Just (CoAx s)
  | otherwise = case tdSolveRewrite s of
    Nothing -> case tdSolveMono s of
      Nothing -> case tdSolveFocus s of
        Nothing -> case tdSolveRes tdSolveResHelperList s of
          Nothing -> Nothing
          pt -> pt
        pt -> pt
      pt -> pt
    pt -> pt

-- tdSolveHelperSpecial - master-solver following a residuation step to prevent
-- residuation looping
tdSolveHelperSpecial :: Residuation -> Sequent -> Maybe ProofTree
tdSolveHelperSpecial res s
  | isAx s = Just (Ax s)
  | isCoAx s = Just (CoAx s)
  | otherwise = case tdSolveRewrite s of
    Nothing -> case tdSolveMono s of
      Nothing -> case tdSolveFocus s of
        Nothing -> case tdSolveRes (tdSolveResHelperPermit res) s of
          Nothing -> Nothing
          pt -> pt
        pt -> pt
      pt -> pt
    pt -> pt

-- tdSolveRewrite - solves rewriting
tdSolveRewrite :: Sequent -> Maybe ProofTree
tdSolveRewrite s = let
  list = [(rewriteLi, RewriteL), (rewriteRi, RewriteR)]
  complist = map (\(f,o) -> (f s, o)) list
  res = dropWhile (\(ns, _) -> s == ns) complist in
  case res of
    ((ns, o):_) -> case tdSolve ns of
      (Just pt) -> Just (Unary s o pt)
      otherwise -> Nothing
    [] -> Nothing

-- tdSolveFocus - solves focusing
tdSolveFocus :: Sequent -> Maybe ProofTree
tdSolveFocus s = let
  list = [(idefocusR, DeFocusR), (idefocusL, DeFocusL), (ifocusR, FocusR),
    (ifocusL, FocusL)]
  complist = map (\(f,o) -> (f s, o)) list
  res = dropWhile (\(ns, _) -> s == ns) complist in
  case res of
    ((ns, o):_) -> case tdSolve ns of
      (Just pt) -> Just (Unary s o pt)
      otherwise -> Nothing
    [] -> Nothing

-- tdSolveMono - solves inverse monotonicity
tdSolveMono :: Sequent -> Maybe ProofTree
tdSolveMono s = let
  list = [(iMonoTensor, MonoTensor), (iMonoSum, MonoSum), (iMonoLDiv, MonoLDiv),
    (iMonoRDiv, MonoRDiv), (iMonoLDiff, MonoLDiff), (iMonoRDiff, MonoRDiff)]
  complist = map (\(f,o) -> (f s, o)) list
  res = dropWhile (\((ns,_), _) -> s == ns) complist in
  case res of
    (((ns1, ns2), o):_) -> case tdSolve ns1 of
      (Just pt1) -> case tdSolve ns2 of
        (Just pt2) -> Just (Binary s o pt1 pt2)
        otherwise -> Nothing
      otherwise -> Nothing
    [] -> Nothing

-- tdSolveRes - solves residuation
tdSolveRes :: [(Sequent -> Sequent, Residuation)] -> Sequent -> Maybe ProofTree
tdSolveRes permit s = let
  complist = map (\(f, o) -> (f s, o)) permit
  residuations = [(ns, o) | (ns,o) <- complist, ns /= s] in
  case tdSolveResHelperOptions residuations of
    Nothing -> Nothing
    (Just (pt, res)) -> Just (Unary s (Res res) pt)

-- tdSolveResHelperOptions - solves residuation, handles the actual branching
tdSolveResHelperOptions :: [(Sequent, Residuation)] ->
  Maybe (ProofTree, Residuation)
tdSolveResHelperOptions [] = Nothing
tdSolveResHelperOptions ((s, res):xs) =
  case tdSolveHelperSpecial (tdSolveResHelperGetInverse res) s of
    Nothing -> tdSolveResHelperOptions xs
    (Just pt) -> Just (pt, res)

-- tdSolveResHelperPermit - calculates new permit-list
tdSolveResHelperPermit :: Residuation -> [(Sequent -> Sequent, Residuation)]
tdSolveResHelperPermit res = filter (\(f, r) -> r /= res) tdSolveResHelperList

-- tdSolveResHelperGetInverse
tdSolveResHelperGetInverse :: Residuation -> Residuation
tdSolveResHelperGetInverse (Res1 i) = Res1i i
tdSolveResHelperGetInverse (Res1i i) = Res1 i
tdSolveResHelperGetInverse (Res2 i) = Res2i i
tdSolveResHelperGetInverse (Res2i i) = Res2 i

-- tdSolveResHelperList - list of functions and operation mappings
tdSolveResHelperList :: [(Sequent -> Sequent, Residuation)]
tdSolveResHelperList = [(f i,o i) | (f,o) <- [(residuate1, Res1),
  (residuate2, Res2), (residuate1i, Res1i), (residuate2i, Res2i)], i <- [1,2]]

{------------------------------------------------------------------------------}
-- Bottom-up solver block
{------------------------------------------------------------------------------}
-- buSolve - master bottom-up solver
-- First argument is the target string/type pair. Second is the master Lexicon
buSolve :: LexItem -> Lexicon -> Maybe ProofTree
buSolve (ts, tt) le = let
  le' = buDeriveLexicon (words ts) le
  pps = buPartial le' in
  case buCombine pps of
    (Just (ps, pt)) -> case buCoerce tt pt of
      (Just pt) -> let
        s = buExtractSequent pt
        s' = focusR s in
        if s' /= s
          then Just (Unary s' FocusR pt)
          else Nothing
      Nothing -> Nothing
    Nothing -> Nothing

-- buDeriveLexicon - Builds the relevant Lexicon for the given target
buDeriveLexicon :: [String] -> Lexicon -> Lexicon
buDeriveLexicon [] _ = []
buDeriveLexicon (w:ws) ls = case buDeriveLexiconHelper w ls of
  [] -> []
  (l:_) -> l : buDeriveLexicon ws ls

-- buDeriveLexiconHelper - Runs the lexicon for each word
buDeriveLexiconHelper :: String -> Lexicon -> Lexicon
buDeriveLexiconHelper "" _ = []
buDeriveLexiconHelper _ [] = []
buDeriveLexiconHelper w (l@(s,_):ls)
  | w == s = [l]
  | otherwise = buDeriveLexiconHelper w ls

-- buPartial - Generates partial proof trees for the derived lexicon
buPartial :: Lexicon -> [PartialProof]
buPartial [] = []
buPartial ((s,t):ls) = case tdSolve (buInsertionSequent t) of
  (Just pt) -> (s,pt) : buPartial ls
  Nothing -> []

-- buInsertionSequent - Generates the "insertion sequent"; the point where a
-- lexical term enters the proof tree
buInsertionSequent :: Formula -> Sequent
buInsertionSequent f = rewriteRi (Neutral (IStruct f) (OStruct f))

-- buCombine - Combines the partial proofs to build a whole proof tree
buCombine :: [PartialProof] -> Maybe PartialProof
buCombine [] = Nothing
buCombine [x] = Just x
buCombine (l:r:xs) = case buCombinePair l r of
  (Just p) -> buCombine (p:xs)
  Nothing -> let
    p = buCombine (r:xs)
    in case p of
      (Just p) -> buCombine (l:[p])
      Nothing -> Nothing

-- buCombinePair - Attempts to combine a pair of partial proofs
buCombinePair :: PartialProof -> PartialProof -> Maybe PartialProof
buCombinePair (ls, lt) (rs, rt) = if buHasInsertion lt || buHasInsertion rt
  then case buCombinePairEval lt rt of
    (Just pt) -> Just (ls ++ " " ++ rs, pt)
    Nothing -> case buCombinePairEval rt lt of
      (Just pt) -> Just (ls ++ " " ++ rs, pt)
      Nothing -> Nothing
  else Nothing

-- buCombinePairEval - Tries to attach the right tree to the left tree
buCombinePairEval :: ProofTree -> ProofTree -> Maybe ProofTree
buCombinePairEval lt rt = let
  ts = buCombineHelperGetType lt
  in case buCombinePairCoerce ts rt of
    (Just (pt, t)) -> case buCombinePairStitch t lt pt of
      (Just pt) -> Just (buRehash pt)
      Nothing -> Nothing
    Nothing -> Nothing

-- buCombinePairCoerce - Tries to coerce all insertion sequent types
buCombinePairCoerce :: [Formula] -> ProofTree -> Maybe (ProofTree, Formula)
buCombinePairCoerce [] _ = Nothing
buCombinePairCoerce (t:ts) pt = case buCoerce t pt of
  (Just pt) -> Just (pt, t)
  Nothing -> buCombinePairCoerce ts pt

-- buCombinePairStitch - Stitches the right tree into the left where the right
-- left tree has an insertion sequent of the given type
buCombinePairStitch :: Formula -> ProofTree -> ProofTree -> Maybe ProofTree
buCombinePairStitch t (Unary x o (Unary s DeFocusL lt)) rt = if
  buHelperExtractType s == Just t
  then Just (Unary x o rt)
  else case buCombinePairStitch t lt rt of
    (Just pt) -> Just (Unary x o (Unary s DeFocusL pt))
    Nothing -> Nothing
buCombinePairStitch t (Unary x o (Unary s DeFocusR lt)) rt = if
  buHelperExtractType s == Just t
  then Just (Unary x o rt)
  else case buCombinePairStitch t lt rt of
    (Just pt) -> Just (Unary x o (Unary s DeFocusR pt))
    Nothing -> Nothing
buCombinePairStitch t (Unary x o pt) rt = case buCombinePairStitch t pt rt of
  (Just pt) -> Just (Unary x o pt)
  Nothing -> Nothing
buCombinePairStitch t (Binary x o pt1 pt2) rt = case
  buCombinePairStitch t pt1 rt of
  (Just pt) -> Just (Binary x o pt pt2)
  Nothing -> case buCombinePairStitch t pt2 rt of
    (Just pt) -> Just (Binary x o pt1 pt)
    Nothing -> Nothing
buCombinePairStitch _ _ _ = Nothing

-- buCombineHelperGetType - Gets the desired type of all insertion sequents
buCombineHelperGetType :: ProofTree -> [Formula]
buCombineHelperGetType (Unary _ _ (Unary s DeFocusL pt)) = case
  buHelperExtractType s of
  (Just t) -> t : buCombineHelperGetType pt
  Nothing -> buCombineHelperGetType pt
buCombineHelperGetType (Unary _ _ (Unary s DeFocusR pt)) = case
    buHelperExtractType s of
    (Just t) -> t : buCombineHelperGetType pt
    Nothing -> buCombineHelperGetType pt
buCombineHelperGetType (Unary _ _ pt) = buCombineHelperGetType pt
buCombineHelperGetType (Binary _ _ pt1 pt2) = case
  buCombineHelperGetType pt1 of
  [] -> case buCombineHelperGetType pt2 of
    [] -> []
    l -> l
  l -> l
buCombineHelperGetType _ = []

-- buHelperExtractType -- Extracts the type from the sequent
buHelperExtractType :: Sequent -> Maybe Formula
buHelperExtractType (Neutral _ (OStruct f)) = Just f
buHelperExtractType _ = Nothing

-- buHasInsertion - Recursively searches a proof tree for insertion sequents
buHasInsertion :: ProofTree -> Bool
buHasInsertion (Ax _) = False
buHasInsertion (CoAx _) = False
buHasInsertion (Unary _ _ (Unary _ DeFocusL _)) = True
buHasInsertion (Unary _ _ (Unary _ DeFocusR _)) = True
buHasInsertion (Unary _ _ pt) = buHasInsertion pt
buHasInsertion (Binary _ _ pt1 pt2) = buHasInsertion pt1 || buHasInsertion pt2

-- buCoerce - Attempts to force a proof tree to match a given type
buCoerce :: Formula -> ProofTree -> Maybe ProofTree
buCoerce t pt@(Unary s _ _) = case buCoerceTrivial t pt of
  (Just pt) -> Just pt
  otherwise -> let
    complist = map (\(f, o) -> (f s, o)) tdSolveResHelperList
    residuations = [(ns, o) | (ns,o) <- complist, ns /= s] in
    case buCoerceEval t residuations of
      (Just (s, res)) -> Just (Unary s (Res res) pt)
      Nothing -> Nothing

-- buCoerceTrivial - Tests if coercion is needed
buCoerceTrivial :: Formula -> ProofTree -> Maybe ProofTree
buCoerceTrivial t pt@(Unary s _ _) = case buHelperExtractType s of
  (Just f) -> if f == t
    then Just pt
    else Nothing
  otherwise -> Nothing

-- buCoerceEval - Evaluates a list of residuations
buCoerceEval :: Formula -> [(Sequent, Residuation)] ->
  Maybe (Sequent, Residuation)
buCoerceEval _ [] = Nothing
buCoerceEval t ((s, res):xs) = case buHelperExtractType s of
  (Just f) -> if f == t
    then Just (s, res)
    else buCoerceEval t xs
  otherwise -> Nothing

-- buRehash - Recursively rebuilds a proof tree to match combined trees
buRehash :: ProofTree -> ProofTree
buRehash pt = case pt of
  Ax {} -> pt
  CoAx {} -> pt
  Unary {} -> buRehashUnary pt
  Binary {} -> buRehashBinary pt

-- buExtractSequent - Extracts the sequent from a proof-tree step
buExtractSequent :: ProofTree -> Sequent
buExtractSequent (Ax s) = s
buExtractSequent (CoAx s) = s
buExtractSequent (Unary s _ _) = s
buExtractSequent (Binary s _ _ _) = s

-- buRehashUnary - Rehashes unary steps in proof trees
buRehashUnary :: ProofTree -> ProofTree
buRehashUnary (Unary _ o pt) = let
  pt' = buRehash pt
  s = buExtractSequent pt' in
  Unary (buRehashUnaryEval s o) o pt'

-- buRehashUnaryEval - Evaluates new unary sequents
buRehashUnaryEval :: Sequent -> Operation -> Sequent
buRehashUnaryEval s o = case o of
  DeFocusL -> defocusL s
  DeFocusR -> defocusR s
  FocusL -> focusL s
  FocusR -> focusR s
  RewriteL -> rewriteL s
  RewriteR -> rewriteR s
  (Res r) -> case r of
    (Res1 i) -> residuate1 i s
    (Res1i i) -> residuate1i i s
    (Res2 i) -> residuate2 i s
    (Res2i i) -> residuate2i i s

-- buRehashBinary - Rehashes binary steps in proof trees
buRehashBinary :: ProofTree -> ProofTree
buRehashBinary (Binary _ o pt1 pt2) = let
  pt1' = buRehash pt1
  pt2' = buRehash pt2
  l = buExtractSequent pt1'
  r = buExtractSequent pt2' in
  Binary (buRehashBinaryEval l r o) o pt1' pt2'

-- buRehashBinaryEval - Evaluates new binary sequents
buRehashBinaryEval :: Sequent -> Sequent -> Operation -> Sequent
buRehashBinaryEval l r o = case o of
  MonoTensor -> monoTensor l r
  MonoLDiff -> monoLDiff l r
  MonoRDiff -> monoRDiff l r
  MonoSum -> monoSum l r
  MonoLDiv -> monoLDiv l r
  MonoRDiv -> monoRDiv l r
