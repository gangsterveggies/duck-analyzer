{-|
Module      : Duck
Description : An experimental complexity tester
Copyright   : (c) Pedro Paredes, 2016
Maintainer  : gangsterveggies@gmail.com
Stability   : stable

Duck is a library to test the complexity of programs.

The programmer provides one program and one or many complexity
functions that will be hypothesis for the theoretical behavior of the
program. Duck then runs the program multiple times for multiple
instances and compares the theoretical hypothesis with the
experimental data, allowing to determing the complexity closest to the
actual one.
-}

module Duck
       (
         -- * Typeclasses and Types for testing
         Sized(..),
         Cased(..),
         Parameters(..),
         Verbosity(..),
         Report(..),
         FullReport(..),
         GroupReport(..),
         -- * Running functions
         runSingleTest,
         plotReport,
         plotHypothesis,
         testHypothesis,
         testGroup,
         -- * Defaults
         defaultParam,
         -- * Hypothesis
         hypothesis,
         hpc, hp, hpln,
         hp2, hp3, hp4
       ) where

import Duck.Types
import Duck.Hypothesis
import Duck.Plot
import Duck.Analysis
