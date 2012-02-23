module ConcSplit.Leaky (
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
import Util.Iteratee

makeImpl :: Int -> Impl
makeImpl chunkSize = 
    let 
        prettySize = prettyPrintSize chunkSize
        name = "leaky" ++ prettySize
        desc = "leaky impl using iterators (enumerator chunk size of " ++ prettySize ++ ")"
        concsplit_impl files parts = concEnum chunkSize files (splitterIter parts) >> pure () 
    in Impl name concsplit_impl desc

concEnum:: Int -> [Allocator Handle] -> I.Enumerator B.ByteString IO a 
concEnum chunkSize files2join splitty= do
    let concEnum' ::[Allocator Handle] -> I.Enumerator B.ByteString IO a 
        concEnum' [] ioiter = pure ioiter
        concEnum' (h:hs) ioiter = do
             resultIter <- CIO.bracket h
                                       snd
                                       (\(handle,release) -> enumHandle chunkSize handle ioiter)
--           resultIter <- CIO.bracket (liftIO h)
--                                     (liftIO . snd)
--                                     (\(handle,release) -> enumHandle chunkSize handle ioiter)
             concEnum' hs resultIter
    concEnum' files2join splitty

splitterIter::[(Allocator Handle,Int)] -> I.Iteratee B.ByteString IO ()
splitterIter [] = return ()
splitterIter ((allocator,size):xs) = do
    (handle,release) <- liftIO allocator
    --liftIO $ putStrLn $ show size
    --liftIO $ threadDelay (1*1000^2)
    cappedIterHandle size handle
    liftIO release
    splitterIter xs
