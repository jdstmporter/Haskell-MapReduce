{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Control.Monad2.Trans.MapReduce (
        MapReduceT, return, (>>=), bind, lift, mr)
where

import qualified Prelude as P
import Prelude hiding(return, (>>=), (.), id)
import Data.Monoid
import Control.Category
import Control.Monad2

-- | The 'MapReduceT' type.  Wraps a function @('Monad' m) => (m a -> m b)@.
data (Monad m) => MapReduceT m a b = MR { run :: m a -> m b }

-- | utility method to lift a function into the 'MapReduceT' monad
mr :: (Monad m) => (m a -> m b) -- ^ the function to lift 
        -> MapReduceT m a b     -- ^ the resulting 'MapReduceT m a b' instance
mr = MR 

-- | '>>>' is just functional composition and 'id' is the identity function.
instance (Monad m) => Category (MapReduceT m) where
        -- | Categorical composition is just functional composition
        (.) x y = MR $ (run x) P.. (run y)
        -- | The identity is the identity of function
        id = MR P.id

-- | 'mappend' is just functional composition and 'mempty' is the identity function.
instance (Monad m) => Monoid (MapReduceT m a a) where
        -- | Monoidal composition is just functional composition
        mappend = flip (.) 
        -- | The identity is the identity of function
        mempty  = id

-- | The bind operation; essentially it wraps 'Control.Monad.>>=' in @m@ so that in @bind p f g@ 
--   the combination @f '>>>' p@ selects the left hand side of 'Control.Monad.>>='. The projection 
--   operator @p@ allows global filtering of
--   the left hand side.  So, for classic MapReduce @p = MR 'nub'@. 
--
--   The implementation of '>>=' in this package is @('>>=') = 'bind' 'id'@.         
bind :: (Monad m) => MapReduceT m b b   -- ^ The projection operator, should be idempotent, so @p.p == p@
        -> MapReduceT m a b             -- ^ first argument
        -> (b -> MapReduceT m b c)      -- ^ second argument
        -> MapReduceT m a c
bind p f g = MR (\ x -> (left x) P.>>= (right x))
                where
                left x = (f >>> p) -< x
                right x y = (f >>> g y) -< x
                (-<) x y = (run x) y

-- | 'return' lifts a value @x@ to the constant function whose value in @m.a@ is @'return' x@, 
--   while '>>=' is taken to be 'bind' with the identity projection operator.                        
instance (Monad m) => Monad2 (MapReduceT m) a b
        where
        -- ^ lifts a value @x@ to the constant function whose value in @m.a@ is @'return'x@
        return x = MR (const $ P.return x)
        -- ^ standard '>>=' implementation takes the projection operator to be the identity
        (>>=) = bind id
        
-- | 'lift' lifts a value in @m a@ to the constant function taking that value        
instance MonadTrans2 MapReduceT 
        where
        -- ^ lifts a value in @m a@ to the constant function taking that value
        lift x = MR (const x)
        
     
