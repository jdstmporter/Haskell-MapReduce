-- | Functions to make test dictionaries and messages.  The key function
--   is 'makeWords' which take as arguments a tuple @(nWords,nVocab)@ where
--   @nWords@ is the desired number of words and @nVocab@ is the size of the
--   vocabulary.  The function uses as words randomly-generated strings of
--   upper-case letters  provided by 'makeWord': one could plug in a pre-existing 
--   if that were felt necessary.
module Parallel.MapReduce.WordCount.Documents (
        makeDictionary,makeSequence,makeWords
        ) where

import Test.QuickCheck(Gen,Positive,choose)
import Control.Monad (liftM2)

                
-- | Produce a string of upper-case characters of random length between 2 and 10.
--   The resulting string is wrapped in 'Gen' because (for simplicity) the entropy
--   source is 'choose'.
makeWord :: Gen String
makeWord = do
        n <- choose(2,10)::Gen Int
        word n
        where
        word 0 = return ""
        word n = liftM2 (:) (choose ('A', 'Z')) (word $ n-1)

-- | Make a dictionary with at most the specified number of words.  Note that there
--   is no checking to avoid duplicates: output is taken straight from 'makeWord'.                   
makeDictionary :: Int   -- ^ The maximum number of words in the dictionary
        -> Gen [String] -- ^ The dictionary wrapped in 'Gen'
makeDictionary 0 = return []
makeDictionary n = liftM2 (:) makeWord $ makeDictionary (n-1)

-- | Make a sequence of specified length of integers in the range @[0..(m-1)]@ for 
--   specified @m@. 
makeSequence :: Int     -- ^ The range for the integer values 
        -> Int          -- ^ The number of values required 
        -> Gen [Int]    -- ^ The values wrapped in 'Gen'        
makeSequence _ 0 = return []
makeSequence m n = liftM2 (:) (choose (0,m-1)) $ makeSequence m (n-1)  

-- | Make a list of a specified number of random words drawn from a dictionary of 
--   at most a specified size.        
makeWords :: Positive Int       -- ^ The number of words to generate
        -> Positive Int         --   The maximum vocabulary size                          
        -> Gen [String]         -- ^ The word list wrapped in 'Gen'
makeWords n m = if 
        (n<=0) || (m <=0) 
                then return []
                else do
                        -- This is important: 'makeDictionary' and 'makeSequence' must be 
                        -- able to take 0 as an argument
                        let m' = fromIntegral m 
                        let n' = fromIntegral n
                        dict <- makeDictionary m' 
                        seq <- makeSequence m' n'
                        return $ map (\i -> dict!!i) seq
                        
