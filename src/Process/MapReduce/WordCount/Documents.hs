-- | Functions to make test dictionaries and messages.  The key function
--   is 'makeWords' which take as arguments a tuple @(nWords,nVocab)@ where
--   @nWords@ is the desired number of words and @nVocab@ is the size of the
--   vocabulary.  The function uses as words randomly-generated strings of
--   upper-case letters  provided by 'makeWord': one could plug in a pre-existing 
--   if that were felt necessary.
module Process.MapReduce.WordCount.Documents (
-- * Types
        BoundedInt,get,
-- * Functions
        makeDictionary,makeSequence,makeWords
        ) where

import Test.QuickCheck(Gen,Arbitrary,arbitrary,choose)
import Control.Monad (liftM2)

-- | Type used to generate tests sets of reasonably small size (as the tests
--   can otherwise be very slow).  A very simple wrapper round 'Int'.
data BoundedInt = I {get :: Int -- ^ Accessor function to unwrap the type
        }
        deriving (Eq,Ord,Show)

-- | Declare 'BoundedInt' an instance of 'Bounded' to set the bounds. 'minBound'
--   must be positive, and if 'maxBound' is too large, the tests will be very slow.
instance Bounded BoundedInt where
        minBound = I 10         
        maxBound = I 10000


-- | The generator instance that produces test sets consisting of random pairs of 
--   'BoundedInt's.
instance Arbitrary BoundedInt where
        arbitrary = do
                x <- choose (get minBound,get maxBound)
                return $ I x
                
-- | Produce a string of upper-case characters of random length between 2 and 10.
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
makeWords :: (BoundedInt,BoundedInt)  -- ^ @(nWords,maxVocab)@ where @nWords@ is the number of words
                        --   required, and @maxVocab@ is the maximum vocabulary size 
        -> Gen [String] -- ^ The message wrapped in 'Gen'
makeWords (I n,I m) = if (n<=0) || (m <=0) 
                then return []
                else do
                        dict <- makeDictionary m
                        seq <- makeSequence m n
                        return $ map (\i -> dict!!i) seq
