
module MRCode where

import Process.MapReduce.Multicore
import Prelude hiding (return,(>>=))
import qualified Prelude as P

countWords :: [(String,Int)] -> Int
countWords = foldr ((+).snd) 0 

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