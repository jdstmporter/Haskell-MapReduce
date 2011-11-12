{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

-- | Module containing the 'Monad2' and 'MonadTrans2' class definitions.  Put simply, these are
--   monads with 2 type arguments, so if @m@ is a 'Monad2' type then an instance of it has type
--   @m a b@ for types @a@ and @b@.  Similarly, if @t@ is a 'MonadTrans2' and 'm' is a 
--   'Monad' then an instance of the transformer has type 't m a b'.
--
--   ['Monad2']  'Monad2' generalises 'Monad' and defines two functions:
--
--   * @'return' :: ('Monad2' m) => a -> m a a@
--
--   * @('>>=') :: ('Monad2' m) => m a b -> (b -> m b c) -> m a c@
--
--   The only really surprising feature is that in '>>=' the pairs of types chain, so we 
--   combine a @m a b@ and a @m b c@ to get a @m a c@.  The function '>>' is defined in
--   the obvious way by analogy to 'Monad'.
--
--   ['MonadTrans2']  'MonadTrans2' generalises 'MonadTrans' and defines one function:
--
--   * @'lift' :: ('MonadTrans2' t,'Monad' m) => m a -> t m a a@
--
--   There is also one constraint: 
--
--   * If @t :: 'MonadTrans2'@ and @m :: 'Monad'@ then @t m :: 'Monad2'@.
--
--  /Relation to monads/: note that if 'm' is a 'Monad2' and define @n a = m a a@
--  and carry across |return| and |>>=| as is, then 'n' is a 'Monad'. 
module Control.Monad2  (
        Monad2,
        MonadTrans2,
        (>>=), return, lift
) where

import Prelude hiding (return,(>>=))

-- | The generalised monad type; simply a monad @m a b@ with two arguments and the operations
--   @return@ and @>>=@ acting so @>>=@ sends a @m a b@ and a @b -> m b c@ 
--   to a @m b c@, so parameter types chain in the obvious way.  
class Monad2 m a b where
        -- | wraps a value in the monad
        return :: a -> m a a  
        -- | monadic bind with obvious generalisation  
        (>>=)  :: m a b -> (b -> m b c) -> m a c 
        -- | bind that throws away the output of the first argument
        (>>) :: m a b -> m b c -> m a c
        (>>) x y = x >>= const y

-- | The generalised monad transformer; takes a 'Monad' and produces a 'Monad2'.
class MonadTrans2 t where
        -- | the transformer lift operation
        lift :: (Monad m) => m a -> t m a a  

-- | If @t@ is a 'MonadTrans2' and @m@ is a 'Monad' then @t m@ must be a 'Monad2'.
--   The instance implementation is deliberately left empty.  This forces 
--   implementers of 'MonadTrans2' to provide the monadic functions.
instance (Monad m,MonadTrans2 t) => Monad2 (t m) a b

        
