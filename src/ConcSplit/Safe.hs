{-# LANGUAGE RankNTypes #-}
module ConcSplit.Safe (
        makeImpl
    ) where 

import System.IO
import qualified Data.ByteString as B
import Control.Monad
import Control.Monad.IO.Class
import Control.Applicative
import Control.Arrow (first,second) -- only to map over tuples with first and second
import Control.Exception
import ConcSplit
import Control.Concurrent
import Data.List
import Util.Parser
import qualified Data.List as L
import qualified Data.Iteratee as I
import Data.Iteratee.IO.Handle
import Data.Iteratee ((><>),(<><))
import Util.Iteratee
import Util.Allocator

makeImpl :: Int -> Impl
makeImpl chunkSize = 
    let 
        prettySize = prettyPrintSize chunkSize
        name = "safe" ++ prettySize
        desc = "iteratee-based, should work in all cases (preferred chunk size of " ++ prettySize ++ ")"
    in Impl name (concsplit_impl chunkSize) desc

concsplit_impl:: Int -> [Allocator Handle] -> Int -> [Allocator Handle] -> IO ()
concsplit_impl chunkSize files2join partSize parts = 
    let mkIter :: Allocator Handle -> Allocator ByteIter 
        mkIter = fmap (first (cappedIterHandle partSize))

        chunkSize' = min chunkSize partSize 

        -- Prepends a ByteIter (possibly containing leftover input) to
        -- the head of a list of as-yet unallocated ByteIters.
        fuse::ByteIter -> [Allocator ByteIter] -> [Allocator ByteIter] 
        fuse iter iterAllocs = (first ((>>) iter) <$> head iterAllocs) : tail iterAllocs

        gomasked:: (forall a. IO a -> IO a) -> -- restore function to run computations unmasked
                   [Allocator ByteIter] -> -- this list of destinations is assumed to be infinite
                   [Allocator Handle] -> 
                   IO () 
        gomasked restore destinations sources = 
            let writeToIter handle iter = do 
                    iter' <- restore $ enumHandle chunkSize' handle iter 
                    atEOF <- hIsEOF handle
                    if atEOF then return (Right iter',\hRelease iRelease -> (hRelease,iRelease))
                             else return (Left (handle,iter'),\hRelease iRelease -> (iRelease,hRelease))

                go _ destinations [] = head destinations >>= snd -- we release the handle underneath the iteratee
                go allocStrategy destinations (source:sources) = do
                    (result,cleanup) <- allocStrategy writeToIter source (head destinations)
                    case result of 
                            -- We are creating a "fake" allocator from an already-allocated iteratee,
                            -- and we use combine-flipped to avoid the danger that an exception
                            -- while opening the source file leaves the destination handle unclosed. 
                            -- With combine_flipped, the cleanup handler for the destination file
                            -- is set up before the cleanup handler for the source file. 
                        Right iter' -> go combine_flipped (pure (iter',cleanup):tail destinations) sources 
                            -- We prepend the iteratee containing leftover input to the list of destinations,
                            -- and we prepend a "fake" allocator with the already-opened file to the list
                            -- of sources.
                        Left (handle,iter') -> go combine (fuse iter' . tail $ destinations) (pure (handle,cleanup):sources)
            in  go combine destinations sources
    in mask $ \restore -> gomasked restore (map mkIter parts) files2join 

