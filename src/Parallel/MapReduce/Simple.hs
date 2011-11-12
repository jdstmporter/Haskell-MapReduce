{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

-- | Module that defines the 'MapReduce' monad and exports the necessary functions.
--
--   Mapper / reducers are generalised to functions of type 
--   @a -> ([(s,a)] -> [(s',b)])@ which are combined using the monad's bind 
--   operation.  The resulting monad is executed on initial data by invoking
--   'runMapReduce'.
--
--   For programmers only wishing to write conventional map / reduce algorithms,
--   which use functions of type @([s] -> [(s',b)])@ a wrapper function
--   'liftMR' is provided, which converts such a function into the
--   appropriate monadic function. 
module Parallel.MapReduce.Simple (
-- * Types
        MapReduce, 
-- * Functions
--        
-- ** Monadic operations
        return, (>>=), 
-- ** Helper functions
        run, distribute, lift) where

import Data.List (nub)
import Control.Applicative ((<$>))
import Control.Monad (liftM)
import Control.DeepSeq (NFData)
import System.IO
import Prelude hiding (return,(>>=))
import Data.Digest.Pure.MD5
import Data.Binary
import qualified Data.ByteString.Lazy as B
import Control.Parallel.Strategies (parMap, rdeepseq)

-- | The parallel map function; it must be functionally identical to 'map',
--   distributing the computation across all available nodes in some way.
pMap :: (NFData b) => (a -> b)  -- ^ The function to apply
        -> [a]                  -- ^ Input 
        -> [b]                  -- ^ output                                                    
pMap = parMap rdeepseq

-- | Generalised version of 'Monad' which depends on a pair of 'Tuple's, both
--   of which change when '>>=' is applied.           
class MonadG m where
        return :: a                     -- ^ value.
                -> m s x s a            -- ^ transformation that inserts the value 
                                        --   by replacing all 
                                        --   the key values with the specified 
                                        --   value, leaving the data unchanged.


        (>>=)  :: (Eq b,NFData s'',NFData c) => 
                m s a s' b              -- ^ Initial processing chain
                -> ( b -> m s' b s'' c )-- ^ Transformation to append to it 
                -> m s a s'' c          -- ^ Extended processing chain


-- | The basic type that provides the MapReduce monad (strictly a generalised monad).  
-- In the definition
-- @(s,a)@ is the type of the entries in the list of input data and @(s',b)@ 
-- that of the entries in the list of output data, where @s@ and @s'@ are data
-- and @a@ and @b@ are keys.
--
-- 'MapReduce' represents the transformation applied to data by one or more
--  MapReduce staged.  Input data has type @[(s,a)]@ and output data has type
--  @[(s',b)]@ where @s@ and @s'@ are data types and @a@, @b@ are key types.
--
--  Its structure is intentionally opaque to application programmers.
newtype MapReduce s a s' b = MR { runMR :: [(s,a)] -> [(s',b)] }

-- | Make MapReduce into a 'MonadG' instance
instance MonadG MapReduce where
        return = ret
        (>>=)  = bind
        
-- | Insert a value into 'MapReduce' by replacing all the key values with the
--   specified value, leaving the data unchanged.
ret :: a                                -- ^ value 
        -> MapReduce s x s a            -- ^ transformation that inserts the value 
                                        --   into 'MapReduce' by replacing all 
                                        --   the key values with the specified 
                                        --   value, leaving the data unchanged. 
ret k = MR (\ss -> [(s,k) | s <- fst <$> ss])

-- ^ Apply a generalised mapper / reducer to the end of a chain of processing
--   operations to extend the chain.
bind :: (Eq b,NFData s'',NFData c) => 
                MapReduce s a s' b      -- ^ Initial state of the monad
        -> (b -> MapReduce s' b s'' c)  -- ^ Transformation to append to it 
        -> MapReduce s a s'' c          -- ^ Extended transformation chain
bind f g = MR (\s ->
        let
                fs = runMR f s
                gs = map g $ nub $ snd <$> fs
        in
        concat $ pMap (`runMR` fs) gs)

-- | Execute a MapReduce MonadG given specified initial data.  Therefore, given
--   a 'MapReduce' @m@ and initial data @xs@ we apply the processing represented
--   by @m@ to @xs@ by executing
--
--   @run m xs@
run :: MapReduce s () s' b              -- ^ 'MapReduce' representing the required processing
                -> [s]                  -- ^ Initial data
                -> [(s',b)]             -- ^ Result of applying the processing to the data
run m ss = runMR m [(s,()) | s <- ss]

-- | The hash function.  Computes the MD5 hash of any 'Hashable' type
hash :: (Binary s) => s         -- ^ The value to hash
        -> Int                  -- ^ its hash
hash s = sum $ map fromIntegral (B.unpack h) 
        where
        h = encode (md5 $ encode s)

-- | Function used at the start of processing to determine how many threads of processing
--   to use.  Should be used as the starting point for building a 'MapReduce'.
--   Therefore a generic 'MapReduce' should look like
--
--   @'distribute' '>>=' f1 '>>=' . . . '>>=' fn@
distribute :: (Binary s) => Int         -- ^ Number of threads across which to distribute initial data 
                -> MapReduce s () s Int -- ^ The 'MapReduce' required to do this  
distribute n = MR (\ss -> [(s,hash s `mod` n) | s <- fst <$> ss])

-- | The wrapper function that lifts mappers / reducers into the 'MapReduce'
--   monad.  Application programmers can use this to apply MapReduce transparently
--   to their mappers / reducers without needing to know any details of the implementation
--   of MapReduce.
--
--   Therefore the generic 'MapReduce' using only traditional mappers and
--   reducers should look like
--
--   @'distribute' '>>=' 'lift' f1 '>>=' . . . '>>=' 'lift' fn@  
lift :: (Eq a) => ([s] -> [(s',b)])     -- traditional mapper / reducer of signature
                                        --  @([s] -> [(s',b)]@
        -> a                            -- the input key 
        -> MapReduce s a s' b           -- the mapper / reducer wrapped as an instance
                                        -- of 'MapReduce'
lift f k = MR (\ss -> f $ fst <$> filter (\s -> k == snd s) ss)




        
        




