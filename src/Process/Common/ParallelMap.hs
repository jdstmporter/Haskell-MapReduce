-- | Simple module that exports a parallelised version of 'Prelude.map'
module Process.Common.ParallelMap (
-- * Functions
        map) where

import Control.Parallel.Strategies (parMap, rdeepseq)
import Control.DeepSeq (NFData)
import Prelude hiding (map)

-- | The parallel map function; it must be functionally identical to 'map',
--   distributing the computation across all available nodes in some way.
map :: (NFData b) => (a -> b) -> [a] -> [b] -- ^ Applies the given function to 
                                            -- data.  The output type must be
                                            -- in the class 'NFData'
                                            -- to ensure that it can have a 
                                            -- parallelisation strategy applied
                                            -- to it.
map = parMap rdeepseq
