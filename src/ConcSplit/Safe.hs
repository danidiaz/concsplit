{-# LANGUAGE RankNTypes #-}
module ConcSplit.Safe (
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
        name = "safe" ++ prettySize
        desc = "safe impl using iterators (enumerator chunk size of " ++ prettySize ++ ")"
    in Impl name (concsplit_impl chunkSize) desc

type MkBi' = MkBi Handle (I.Iteratee B.ByteString IO ())

concsplit_impl:: Int -> [Allocator Handle] -> [(Allocator Handle,Int)] -> IO ()
concsplit_impl chunkSize files2join parts = 
    let mkIter (allocator,size) = allocator >>= \(handle,cleanup) -> return (cappedIterHandle size handle,cleanup) 

        writeToIter handle iter = enumHandle chunkSize handle iter 

        go:: (forall a. IO a -> IO a) -> MkBi' -> [Allocator (I.Iteratee B.ByteString IO ())] -> [Allocator Handle] -> IO () 
        go restore allocStrategy destinations sources = 
            let go' _ destinations [] = head destinations >>= snd 
                go' allocStrategy destinations (source:sources) = do 
                    (handle,releaseHandle,iter,releaseIter) <- allocStrategy source (head destinations) 
                    iter' <- onException (onException (restore $ writeToIter handle iter) releaseIter) releaseHandle       
                    atEOF <- hIsEOF handle
                    if atEOF
                        then do onException releaseHandle releaseIter
                                go' allocRightToLeft (pure (iter',releaseIter):tail destinations) sources 
                        else do onException releaseIter releaseHandle
                                go' allocLeftToRight (tail destinations) (pure (handle,releaseHandle):sources)
            in go' allocStrategy destinations sources
    in mask $ \restore -> go restore allocLeftToRight (map mkIter parts) files2join 
