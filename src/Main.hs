-- | The wordcount test program
module Main where


import System.IO
import System.Environment (getArgs)
import Test.QuickCheck (Property,Result (Failure),Positive,quickCheckResult)
import Process.MapReduce.WordCount.Tests (prop_Equal,prop_FullCheck)

applyTest :: ((Positive Int,Positive Int) -> Property) -> IO Int
applyTest f = do
        r <- quickCheckResult f
        case r of
                Failure _ _ _ _ _ _ _ -> return 1
                _ -> return 0
 
-- Apply some tests to wordcount
main::IO Int
main  = do
        b <- do
                a <- getArgs
                if length a > 0
                        then return $ read (head a)
                        else return 10000      
        putStrLn $ "Testing the word-count MapReduce algorithm with size bound " ++ show (fromIntegral b)
        putStrLn "WARNING: Can be very slow if bound > 10000"          
        putStrLn "Total count test (coarse-grained): " 
        r1 <- applyTest (prop_Equal b)
        putStrLn "Detailed test (fine-grained): "
        r2 <- applyTest (prop_FullCheck b)
        return $ r1 + r2 
        
