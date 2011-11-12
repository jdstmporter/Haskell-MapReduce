-- |  Module defining two 'quickCheck' test methods for the MapReduce 
--    wordcount algorithm.
--
--    [prop_Equal] A test which uses MapReduce to do wordcount, then 
--    compares its predicted total number of words with the actual total 
--    number
--
--    [prop_fullCheck] A test which uses MapReduce to do wordcount, 
--    then compares its results from another, more elementary algorithm
module Parallel.MapReduce.WordCount.Tests (
        prop_Equal,
        prop_FullCheck
        ) where

import Test.QuickCheck
import Parallel.MapReduce.WordCount(mapReduce)
import Parallel.MapReduce.WordCount.Documents(makeWords)


-- | Utility function to reduce a @('Positive' 'Int','Positive' 'Int')@ modulo 
--   some @'Positive' 'Int'@
shrinkIt :: Positive Int                       -- ^ the base to reduce modulo
        -> (Positive Int,Positive Int)         -- ^ the pair to reduce
        -> (Positive Int,Positive Int)
shrinkIt b (n,m) = (n `rem1` b, m `rem1` b)
        where 
        rem1 x b = 1 + (x `rem` b)

       

-- #####################################################################
-- THE TESTS: TEST 1
-- #####################################################################

-- | Given a @[('String','Int')]@ of counts from 'mapReduce' compute the
--   total count, which is the sum of all the 'Int' values.
countWords :: [(String,Int)]    -- ^ The counts
        -> Int                  -- ^ Their total
countWords = foldr ((+).snd) 0 

-- | Simple test that 'mapReduce' returns the correct total number of words,
--   that is that the sum of all of its counts equals the total number of
--   words it was given.
prop_Equal :: Positive Int              -- ^ The upper bound on test size 
        -> (Positive Int,Positive Int)  -- ^ The proposed test set
        -> Property                     -- ^ Whether the test passed
prop_Equal b p = forAll (makeWords n m) $ \words -> 
        countWords (mapReduce 16 words) == fromIntegral n
        where 
        (n,m) = shrinkIt b p
        
-- #####################################################################
-- THE TESTS: TEST 2
-- #####################################################################
        
-- | Count the number of times a given value appears in a list.
countIn :: (Eq a) => a  -- ^ The value
        -> [a]          -- ^ The list
        -> Int          -- ^ The count
countIn x xs = foldr f 0 xs
        where
        f y n 
                | x == y = n+1
                | otherwise = n
-- | Turn a list of strings into @('String','Int')@ pairs giving
--   the counts of appearances.
makeCounts :: [String]          -- ^ The input list
        -> [(String,Int)]       -- ^ The word / count pairs
makeCounts xs = count xs []
        where
        count [] ys = ys
        count (x:xs) ys = case lookup x ys of
                Nothing -> count xs ((x,1):ys)
                Just n -> count xs ((x,n+1):filter (\p -> fst p /= x) ys)
                 
-- | Compare the corpus of words with the counts produced by 'mapReduce'.
testCount :: [String]           -- ^ The corpus
        -> [(String,Int)]       -- ^ Counts from 'mapReduce'
        -> Bool                 -- ^ Whether they agree
testCount ss cs = compare (makeCounts ss) cs
        where 
        compare [] [] = True
        compare [] _ = False
        compare (x:xs) cs = case lookup (fst x) cs of
                Nothing -> False
                Just y -> (snd x == y) && compare xs (filter (/= x) cs) 
        
-- | More complex test to verify the counts 'mapReduce' produces on a word-by-word
--   basis.  
prop_FullCheck :: Positive Int          -- ^ The upper bound on test size 
        -> (Positive Int,Positive Int)  -- ^ The proposed test set
        -> Property                     -- ^ Whether the test passed
prop_FullCheck b p = forAll (makeWords n m) $ \words -> 
        testCount words (mapReduce 16 words)
        where 
        (n,m) = shrinkIt b p  