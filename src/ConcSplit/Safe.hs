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
    let mkIter = fmap (first (cappedIterHandle partSize))

        chunkSize' = min chunkSize partSize 

        fuse iter iterAllocs = (first ((>>) iter) <$> head iterAllocs) : tail iterAllocs

        gomasked:: (forall a. IO a -> IO a) -> -- restore function to run computations unmasked
                   [Allocator ByteIter] -> -- this list is assumed to be infinite
                   [Allocator Handle] -> 
                   IO () 
        gomasked restore destinations sources = 
            let writeToIter handle iter = do 
                    iter' <- restore $ enumHandle chunkSize' handle iter 
                    atEOF <- hIsEOF handle
                    if atEOF then return (Right iter',\hRelease iRelease -> (hRelease,iRelease))
                             else return (Left (handle,iter'),\hRelease iRelease -> (iRelease,hRelease))

                go _ destinations [] = head destinations >>= snd 
                go allocStrategy destinations (source:sources) = do
                    (result,cleanup) <- allocStrategy writeToIter source (head destinations)
                    case result of 
                        Right iter' -> go combine_flipped (pure (iter',cleanup):tail destinations) sources 
                        Left (handle,iter') -> go combine (fuse iter' . tail $ destinations) (pure (handle,cleanup):sources)
            in  go combine destinations sources
    in mask $ \restore -> gomasked restore (map mkIter parts) files2join 

