-- | The wordcount test program
module Main where


import System.IO
import System.Environment (getArgs)
import Test.QuickCheck (quickCheck)
import Process.MapReduce.WordCount.Tests (prop_Equal,prop_FullCheck)
 
-- Apply some tests to wordcount
main::IO()
main  = do
        b <- do
                a <- getArgs
                if length a > 0
                        then return $ read (head a)
                        else return 10000      
        putStrLn $ "Testing the word-count MapReduce algorithm with size bound " ++ show (fromIntegral b)
        putStrLn "WARNING: Can be very slow if bound > 10000"          
        putStrLn "Total count test (coarse-grained): " >> quickCheck (prop_Equal b)
        putStrLn "Detail test (very strong): " >> quickCheck (prop_FullCheck b)
