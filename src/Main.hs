-- | Classic word count MapReduce algorithm written with the monad
--
--   Takes as argument:
--
--   * A file of word-based text
--
--   * (Optional) the number of mappers to use in the first stage
--     (defaults to 16)
module Main where

import Process.MapReduce.Multicore
import System.IO
import System.Environment (getArgs)
import Prelude hiding (return,(>>=))
import qualified Prelude as P

showNice :: [(String,Int)] -> IO()
showNice [] = P.return ()
showNice (x:xs) = do
        putStrLn $ fst x ++ " occurs "++ show ( snd x) ++ " times"
        showNice xs 

countWords :: [(String,Int)] -> Int
countWords = foldr ((+).snd) 0  

main::IO()
main = do
        args <- getArgs
        out <- case length args of 
                0 -> error "Usage: wordcount [filename] ([num mappers])"
                _ -> do
                        state <- getLines (head args)
                        let nMap = case length args of
                                1 -> 16
                                _ -> read $ args!!1
                        let res = mapReduce nMap state
                        P.return res
        showNice out
        print (countWords out)

-- perform MapReduce

mapReduce :: Int -> [String] -> [(String,Int)]
mapReduce n state = runMapReduce mr state
        where
        mr = distributeMR n >>= liftMR mapper >>= liftMR reducer 

-- transformers

mapper :: [String] -> [(String,String)]
mapper [] = []
mapper (x:xs) = parse x ++ mapper xs
        where
        parse x = map (\w -> (w,w)) $ words x

reducer :: [String] -> [(String,Int)]
reducer [] = []
reducer xs = [(head xs,length xs)]

-- get input

getLines :: FilePath -> IO [String]
getLines file = do
        h <- openFile file ReadMode
        text <- hGetContents h
        P.return (lines text) 
        





         


 
                      