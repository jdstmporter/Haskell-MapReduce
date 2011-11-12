-- | The wordcount test program
module Main where


import System.IO
import System.Environment (getArgs)
import Test.QuickCheck (Property,Result (Failure),Positive,quickCheckResult)
import Control.Monad(mapM,when,sequence)
import Parallel.MapReduce.WordCount.Interface


 
-- Apply some tests to wordcount; returns non-zero on failure
main::IO Int
main  = do
        b <- do
                a <- getArgs
                if length a > 0
                        then return $ read (head a)
                        else return 10000      
        putStrLn $ "Testing the word-count MapReduce algorithm with size bound " ++ show (fromIntegral b)
        when (b > 10000) $ putStrLn "WARNING: Can be very slow if bound > 10000" 
        rs <- mapM (\t -> putStrLn (name t ++ ": ") >> applyTestInt t b) tests
        return $ sum rs  
