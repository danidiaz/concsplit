{-# LANGUAGE RankNTypes #-}
module ConcSplit.Safe (
        makeImpl
    ) where 

import System.IO
import qualified Data.ByteString as B
import Control.Monad
import Control.Monad.IO.Class
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
        desc = "iteratee-based, should work in all cases (preferred chunk size of " ++ prettySize ++ ")"
    in Impl name (concsplit_impl chunkSize) desc

concsplit_impl:: Int -> [Allocator Handle] -> Int -> [Allocator Handle] -> IO ()
concsplit_impl chunkSize files2join partSize parts = 
    let mkIter allocator = allocator >>= \(handle,cleanup) -> return (cappedIterHandle partSize handle,cleanup) 

        chunkSize' = min chunkSize partSize 

        writeToIter handle iter = enumHandle chunkSize' handle iter 

        fuse iter iterAllocs = 
            let fuse' (iter',release) = pure (iter >> iter',release)
            in (head iterAllocs >>= fuse') : tail iterAllocs

        gomasked:: (forall a. IO a -> IO a) -> 
             AllocStrategy Handle ByteIter ->
             [Allocator ByteIter] -> -- this list is assumed to be infinite
             [Allocator Handle] -> 
             IO () 
        gomasked restore allocStrategy destinations sources = 
            let go _ destinations [] = head destinations >>= snd 
                go allocStrategy destinations (source:sources) = do 
                    (handle,releaseHandle,iter,releaseIter) <- allocStrategy source (head destinations) 
                    iter' <- onException (onException (restore $ writeToIter handle iter) releaseIter) releaseHandle       
                    atEOF <- hIsEOF handle
                    if atEOF
                        then do releaseHandle `onException` releaseIter
                                go allocRightToLeft (pure (iter',releaseIter):tail destinations) sources 
                        else do releaseIter `onException` releaseHandle
                                go allocLeftToRight (fuse iter' $ tail destinations) (pure (handle,releaseHandle):sources)
            in go allocStrategy destinations sources
    in mask $ \restore -> gomasked restore allocLeftToRight (map mkIter parts) files2join 
