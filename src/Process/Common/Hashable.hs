{-# LANGUAGE TypeSynonymInstances #-}

-- | A module that defines the machinery required to compute MD5 hashes of 
--   data.  The exported items are:
--
--   * 'Hashable' : a type class of types that are hashable
--
--   * 'hash' : a function that returns the MD5 hash of any 'Hashable'
--
--   The module also makes some standard types members of 'Hashable', specifically
--   'String', 'Integer' and 'ByteString'

module Process.Common.Hashable (
--   * Type Classes
        Hashable (conv), 
--   * Functions        
        hash) where

import qualified Crypto.Hash.MD5 as H
import Data.Char (ord)
import Data.ByteString (ByteString,pack,unpack,singleton)
import Data.Bits (xor,(.&.))

{- THE HASHABLE CLASS -}

-- class definition; any type that can be converted to 'ByteString' is hashable

-- | The class of hashable types.  A type is hashable if it can be converted
--   to a 'ByteString' in a standard way.  The class defines a single
--   method 'conv' to effect this conversion.
class Hashable s where
        conv :: s -> ByteString -- ^ converts a 'Hashable' to a 'ByteString'
        
-- the hash function

-- | The hash function.  Computes the MD5 hash of any 'Hashable' type
hash :: (Hashable s) => s -> Int -- ^ computes the hash
hash s = sum $ map fromIntegral (unpack h) 
        where
        h = H.hash $ conv s        

{- INSTANCES OF THE CLASS -}
        
-- make String Hashable

instance Hashable String where
        conv s = pack $ map (fromIntegral . ord) s
        
instance Hashable Integer where
        conv 0 = singleton 0
        conv x = pack . map fromIntegral $ conv' x
                where
                conv' 0 = []
                conv' y = (y .&. 255):conv' (y `div` 256)

instance Hashable ByteString where
        conv = id         
        
               