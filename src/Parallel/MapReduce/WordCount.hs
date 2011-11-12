-- | Module that uses "Process.MapReduce.Multicore" to implement
--   the standard Mapreduce wordcount algorithm.
module Parallel.MapReduce.WordCount (
        mapReduce
        ) where

--import Parallel.MapReduce.Multicore
import Parallel.MapReduce (run,distribute,lift,(>>=))
import Prelude hiding (return,(>>=))

-- | Perform MapReduce on a list of words, returning word / count pairs
mapReduce :: Int                -- ^ The number of mappers to use on the first stage
        -> [String]             -- ^ The list of words to count    
        -> [(String,Int)]       -- ^ The list of word / count pairs
mapReduce n state = run mr state
        where
        mr = distribute n >>= lift mapper >>= lift reducer 

-- transformers

mapper :: [String] -> [(String,String)]
mapper [] = []
mapper (x:xs) = parse x ++ mapper xs
        where
        parse x = map (\w -> (w,w)) $ words x

reducer :: [String] -> [(String,Int)]
reducer [] = []
reducer xs = [(head xs,length xs)]