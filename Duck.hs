{-|
Module      : Duck
Description : An experimental complexity tester
Copyright   : (c) Pedro Paredes, 2016
Maintainer  : gangsterveggies@gmail.com
Stability   : stable

Duck is a library to test the complexity of programs.

The programmer provides one function and one or many complexity
functions that will be hypothesis for the theoretical behavior of the
function. Duck then evaluates the function multiple times for multiple
instances and compares the theoretical hypothesis with the
experimental data, allowing to determing the complexity closest to the
actual one.
-}

module Duck
       (
         -- * Typeclasses and Types for testing
         -- ** Test case classes
         Sized(..),
         Cased(..),
         -- ** Input and output datas
         Parameters(..),
         Verbosity(..),
         Report(..),
         FullReport(..),
         GroupReport(..),
         Hypothesis,
         -- * Worker functions
         -- ** Generate full reports
         runSingleTest,
         -- ** Hypothesis testing
         testHypothesis,
         testGroup,
         -- ** Plotting
         plotReport,
         plotHypothesis,
         -- * Defaults
         defaultParam,
         outlierReport,
         relevant,
         maxConf,
         topConf,
         allConf,
         -- * Hypothesis
         hypothesis,
         hpc, hp, hpln,
         hp2, hp3, hp4
       ) where

import Duck.Utils
import Duck.Types
import Duck.Hypothesis
import Duck.Plot
import Duck.Statistics
import Duck.Analysis
