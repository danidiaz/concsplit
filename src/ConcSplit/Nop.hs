module ConcSplit.Nop (
        impl,
        concSplitFiles,
        splitHandle
    ) where 

import System.IO
import qualified ConcSplit as CS

impl:: CS.Impl
impl= CS.Impl ["nop"] concSplitFiles splitHandle 

concSplitFiles:: Int -> [FilePath] -> [(FilePath,Int)] -> IO ()
concSplitFiles _ _ _ = return ()

splitHandle:: Int -> Handle -> [(FilePath,Int)] -> IO ()
splitHandle _ _ _ = return ()
