module Duck
       (
         -- Typeclasses and Types for testing
         Testable,
         Test(..),
         Report(..),
         FullReport(..),
         -- Running functions
         runSingleTest,
         plotReport,
         testHypothesis
       ) where

import Duck.Types
import Duck.Plot
import Duck.Analysis
