module ConcSplit.Leaky (
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
        name = "leaky" ++ prettySize
        desc = "iteratee-based, naive, doesn't quite work (preferred chunk size of " ++ prettySize ++ ")"
        concsplit_impl files chunkSize parts = concEnum chunkSize files (splitterIter chunkSize parts) >> pure () 
    in Impl name concsplit_impl desc

concEnum:: Int -> [Allocator Handle] -> ByteEnum a
concEnum chunkSize files2join splitty= do
    let concEnum' ::[Allocator Handle] -> ByteEnum a
        concEnum' [] ioiter = pure ioiter
        concEnum' (h:hs) ioiter = do
             resultIter <- bracket h
                                   snd
                                   (\(handle,release) -> enumHandle chunkSize handle ioiter)
             concEnum' hs resultIter
    concEnum' files2join splitty

splitterIter::Int -> [Allocator Handle] -> ByteIter
splitterIter size allocs =
    let splitterIter' [] = return ()
        splitterIter' (allocator:xs) = do
            (handle,release) <- liftIO allocator
            cappedIterHandle size handle
            liftIO release
            splitterIter' xs
    in splitterIter' allocs
