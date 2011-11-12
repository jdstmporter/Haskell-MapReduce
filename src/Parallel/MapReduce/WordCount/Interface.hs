{-# LANGUAGE FlexibleInstances #-}

-- | Test Interface module for use with Test Agents implementing 
--   "Distribution.TestSuite".  Exports @tests::['ATest']@ containing
--   two tests, one which compares the total number of words computed
--   by the MapReduce algorithm with that provided, and one which
--   computes word counts with a non-MapReduce algorithm and compares
--   results on a word-by-word basis.
module Parallel.MapReduce.WordCount.Interface 
        (
-- * Tests
         tests,
-- * Types
         ATest,
-- * Utilities         
         applyTest,     
         name   
        ) where

import Distribution.TestSuite
import Test.QuickCheck(Positive,Property,quickCheckResult)
import qualified Test.QuickCheck as Q
import Data.Typeable(Typeable,typeOf,mkTyCon,mkTyConApp)
import Control.Applicative ((<$>))
import Parallel.MapReduce.WordCount.Tests        

-- | Need to make 'Positive' 'Typeable' because it isn't by default
instance (Typeable a) => Typeable (Positive a) where
        typeOf x = mkTyConApp (mkTyCon "Positive") [typeOf x]

-- | The 'Test' type, wrapping a test and its familiar name         
data ATest = T {
        -- | The test's name
        testName:: String,
        -- | The test itself
        test:: Positive Int -> (Positive Int,Positive Int) -> Property 
                 } 

-- | Make 'ATest' an instance of 'TestOptions' with one option: /maxSize/ which
--   is a @'Positive' 'Int'@.
instance TestOptions ATest where
        name = testName
        options _ = [("maxSize",typeOf (undefined::(Positive Int)))]
        defaultOptions _ = return $ Options [("maxSize","10000")]
        check _ (Options xs) = fst <$> filter (\(n,v) -> n /= "maxSize") xs

-- | 'ImpureTestable' instance, which is a simple wrapper around 'quickCheckResult'.
--   Note that this has to be /impure/ because 'quickCheckResult' returns results
--   in "System.IO".
instance ImpureTestable ATest where
        runM t o = do
                let b = lookupOption "maxSize" o
                applyTest t b

-- | The array of tests exported for use by "Distribution.TestSuite" aware Test Agents
tests :: [ATest]               
tests = [T {testName="Simple totalising test", test=prop_Equal},
         T {testName="Detailed test", test=prop_FullCheck}
         ]

-- | simple function to apply a test and return a @'IO' 'Result'@.
applyTest :: ATest                                 -- ^ test to apply
        -> Positive Int                            -- ^ bound on test size
        -> IO Result                               -- ^ the result
applyTest t b = do
        r <- quickCheckResult (test t b)
        case r of
                Q.Failure _ _ _ _ _ _ _ -> return $ Fail (name t) 
                _ -> return Pass         