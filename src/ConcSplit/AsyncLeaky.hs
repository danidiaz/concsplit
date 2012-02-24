module ConcSplit.AsyncLeaky (
        makeImpl
    ) where 

import System.IO
import qualified Data.ByteString as B
import Control.Monad
import Control.Applicative
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

makeImpl :: Int -> Impl
makeImpl chunkSize = 
    let 
        prettySize = prettyPrintSize chunkSize
        name = "async-leaky" ++ prettySize
        desc = "less naive, iteratee-based, still vulnerable to asynchronous exceptions (enumerator chunk size of " ++ prettySize ++ ")"
    in Impl name (concsplit_impl chunkSize) desc

concsplit_impl:: Int -> [Allocator Handle] -> [(Allocator Handle,Int)] -> IO ()
concsplit_impl chunkSize files2join parts = 
    let mkIter (allocator,size) = allocator >>= \(handle,cleanup) -> return (cappedIterHandle size handle,cleanup) 

        writeToIter handle iter = enumHandle chunkSize handle iter 

        go:: AllocStrategy Handle ByteIter -> 
             [Allocator ByteIter] -> -- this list is assumed to be infinite
             [Allocator Handle] -> 
             IO () 
        go allocStrategy destinations [] = head destinations >>= snd 
        go allocStrategy destinations (source:sources) = do 
            (handle,releaseHandle,iter,releaseIter) <- allocStrategy source (head destinations) 
            iter' <- onException (onException (writeToIter handle iter) releaseIter) releaseHandle       
            atEOF <- hIsEOF handle
            if atEOF
                then do releaseHandle `onException`  releaseIter
                        go allocRightToLeft (pure (iter',releaseIter):tail destinations) sources 
                else do releaseIter `onException` releaseHandle
                        go allocLeftToRight (tail destinations) (pure (handle,releaseHandle):sources)
    in go allocLeftToRight (map mkIter parts) files2join 
