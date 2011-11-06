-- | The wordcount test program
module Main where

import System.IO
import Test.QuickCheck (quickCheck)
import Process.MapReduce.WordCount.Tests (prop_Equal,prop_FullCheck)
 
-- Apply some tests to wordcount
main::IO()
main  = do
        putStrLn "Testing the word-count MapReduce algorithm"
        putStrLn "WARNING: Can be very slow!"          
        putStrLn "Total count test (coarse-grained): " >> quickCheck prop_Equal
        putStrLn "Detail test (very strong): " >> quickCheck prop_FullCheck
       
                                
                