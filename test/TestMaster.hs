{-
-- LoGoff testcases and testing functions
-- Actual unit testing frameworks are for scrubs
-}

module TestMaster
      (
      main
      ) where

import Control.Monad
import System.Exit

import Logoff
import Datatypes

import TestsAxiom
import TestsShowRead
import TestsFocus
import TestsMono
import TestsRes

{------------------------------------------------------------------------------}
-- Framework operative block
{------------------------------------------------------------------------------}
-- main function
main :: IO ()
main = do
  result <- ioize res
  unless (evaluateResult res) exitFailure
  where res = map evaluateTest tests

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
