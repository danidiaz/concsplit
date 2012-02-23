module ConcSplit.AsyncLeaky (
        makeImpl
    ) where 

import System.IO
import qualified Data.ByteString as B
import Control.Monad
import Control.Applicative
import Control.Monad.CatchIO as CIO
import Control.Monad.IO.Class
import ConcSplit
import Control.Concurrent
import Data.List
import Util.Parser
import qualified Data.List as L
import qualified Data.Iteratee as I
import Data.Iteratee.IO.Handle
import Data.Iteratee ((><>),(<><))

makeImpl :: Int -> Impl
makeImpl chunkSize = 
    let 
        prettySize = prettyPrintSize chunkSize
        name = "async-leaky" ++ prettySize
        desc = "async-leaky impl using iterators (enumerator chunk size of " ++ prettySize ++ ")"
    in Impl name (concsplit_impl chunkSize) desc

type MkBi' = MkBi Handle (I.Iteratee B.ByteString IO ())

concsplit_impl:: Int -> [Allocator Handle] -> [(Allocator Handle,Int)] -> IO ()
concsplit_impl chunkSize files2join parts = 
    let mkIter (allocator,size) = allocator >>= \(handle,cleanup) -> return (cappedIterHandle chunkSize handle,cleanup) 

        writeToIter handle iter = enumHandle chunkSize handle iter 

        go:: MkBi' -> [Allocator (I.Iteratee B.ByteString IO ())] -> [Allocator Handle] -> IO () 
        go allocStrategy destinations [] = head destinations >>= snd 
        go allocStrategy destinations (source:sources) = do 
            (handle,releaseHandle,iter,releaseIter) <- allocStrategy source (head destinations) 
            iter' <- CIO.onException (CIO.onException (writeToIter handle iter) releaseIter) releaseHandle       
            putStrLn "#######"
            atEOF <- hIsEOF handle
            if atEOF
                then do CIO.onException releaseHandle releaseIter
                        go allocRightToLeft (pure (iter',releaseIter):tail destinations) sources 
                else do CIO.onException releaseIter releaseHandle
                        go allocLeftToRight (tail destinations) (pure (handle,releaseHandle):sources)
    in go allocLeftToRight (map mkIter parts) files2join 
