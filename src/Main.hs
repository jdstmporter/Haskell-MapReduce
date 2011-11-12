-- | The wordcount test program
module Main where


import System.IO
import System.Environment (getArgs)
import Test.QuickCheck (Property,Result (Failure),Positive,quickCheckResult)
import Control.Monad(mapM_,when)
import Parallel.MapReduce.WordCount.Interface


 
-- Apply some tests to wordcount; returns non-zero on failure
main::IO ()
main  = do
        b <- do
                a <- getArgs
                if length a > 0
                        then return $ read (head a)
                        else return 10000      
        putStrLn $ "Testing the word-count MapReduce algorithm with size bound " ++ show (fromIntegral b)
        when (b > 10000) $ putStrLn "WARNING: Can be very slow if bound > 10000" 
        mapM_ (\t -> putStrLn (name t ++ ": ") >> applyTest t b) tests
