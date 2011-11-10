{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

-- |This module defines the MapReduce type associated to any 'Monad'.  This
-- involves taking functions @m a -> m b@ where @m@
-- is the 'Monad' being transforned.  
--
-- The 'bind' function is (essentially):
--
-- @'bind' p f g = (\x -> ((f '>>>' p) '-<' x) >>= (\y -> (f '>>>' g y) '-<' x))@
--
-- This can be thought of as using the values of @f '>>>' p@ to /select/ information
-- from @f '>>>' g@ via '>>=' in @m@.  Here @p@ is of type @'MapReduceT m b b@ and should 
-- be /idempotent/, that is
--
-- @p '>>>' p = p@
--
-- It provides the means to apply some filtering or conditioning on the left hand, or 
-- selecting side of the '>>='.  For example in 'Process.MapReduce.Multicore.MapReduce' it
-- is (essentially) @'nub' . 'fst'@, and provides the selection on unique key values
-- that is characteristic of MapReduce. 
--
-- The module exports:
--
-- * The type constructor 'MapReduceT'
--
-- * Monadic functions: 'return' and '>>=', where we define @('>>=') = 'bind' 'id'@ 
--
-- * Transformer functions: 'lift'
--
-- * The general 'bind' function
--
-- * The function 'mr', which lifts functions @m a -> m b@ into 'MapReduceT' 
--
module Control.Monad2.Trans.MapReduce (
-- * Types
        MapReduceT, 
-- * Monadic functions   
        return,     
        (>>=),      
-- * Transformer functions
        lift,       
-- * Other functions
        bind,      
        mr          
        )
where

import qualified Prelude as P
import Prelude hiding(return, (>>=), (.), id)
import Data.Monoid
import Control.Category
import Control.Monad2

-- | The 'MapReduceT' type.  Wraps a function @('Monad' m) => (m a -> m b)@.
data (Monad m) => MapReduceT m a b = MR { run :: m a -> m b }

-- | Utility function to lift a function @m a -> m b@ into 'MapReduceT'.
mr :: (Monad m) => (m a -> m b) -- ^ the function to lift 
        -> MapReduceT m a b     -- ^ the resulting 'MapReduceT m a b' instance
mr = MR 

-- | 'MapReduceT' is a 'Category' with 
--
-- * '>>>' being functional composition
-- 
-- * 'id' being the identity function @m a -> m a@
instance (Monad m) => Category (MapReduceT m) where
        (.) x y = MR $ run x P.. run y
        id = MR P.id

-- | 'MapReduceT' is a 'Monoid' with 
--
-- * 'mappend' being functional composition
-- 
-- * 'mempty' being the identity function @m a -> m a@
instance (Monad m) => Monoid (MapReduceT m a a) where
        mappend = flip (.) 
        mempty  = id

-- | Bind two 'MapReduceT' instances with a specified projection operator: see above          
bind :: (Monad m) => MapReduceT m b b   -- ^ The projection operator, should be idempotent
        -> MapReduceT m a b             -- ^ first argument
        -> (b -> MapReduceT m b c)      -- ^ second argument
        -> MapReduceT m a c
bind p f g = MR (\ x -> left x P.>>= right x)
                where
                left x = (f >>> p) `run` x
                right x y = (f >>> g y) `run` x


-- | Make 'MapReduceT' a 'Monad2'
--
-- * 'return' lifts a value @x@ to the constant function whose value in @m.a@ is @'return'x@
--
-- * '>>=' is just @'bind' 'id'@                      
instance (Monad m) => Monad2 (MapReduceT m) a b
        where
        return x = MR (const $ P.return x)
        (>>=) = bind id

-- | Make 'MapReduceT' a 'MonadTrans2'
--
-- * 'lift' lifts a value in @m a@ to the constant function taking that value                
instance MonadTrans2 MapReduceT 
        where
        lift x = MR $ const x
        
     
