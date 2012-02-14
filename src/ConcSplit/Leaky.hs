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

makeImpl :: Int -> Impl
makeImpl chunkSize = 
    let 
        prettySize = prettyPrintSize chunkSize
        name = "leaky" ++ prettySize
        desc = "leaky impl using iterators (enumerator chunk size of " ++ prettySize ++ ")"
    in Impl name (concsplit_impl chunkSize) desc

-- to do: change concsplit to concEnum, move splitty to impl creation
concsplit_impl:: Int -> [Allocator Handle] -> [(Allocator Handle,Int)] -> IO ()
concsplit_impl chunkSize files2join parts = do
    let splitty = splitterIter parts
        enumy ::[Allocator Handle] -> I.Enumerator B.ByteString IO ()  
        enumy [] ioiter = pure ioiter
        enumy (h:hs) ioiter = do
             resultIter <- CIO.bracket h
                                       snd
                                       (\(handle,release) -> enumHandle chunkSize handle ioiter)
--           resultIter <- CIO.bracket (liftIO h)
--                                     (liftIO . snd)
--                                     (\(handle,release) -> enumHandle chunkSize handle ioiter)
             enumy hs resultIter
    enumy files2join splitty >> pure ()

splitterIter:: [(Allocator Handle,Int)] -> I.Iteratee B.ByteString IO ()
splitterIter [] = return ()
splitterIter ((allocator,size):xs) = do
    (handle,release) <- liftIO allocator
    liftIO $ putStrLn $ show size
    liftIO $ threadDelay (1*1000^2)
    takeNIterHandle size handle
    liftIO release
    splitterIter xs

takeNIterHandle:: Int -> Handle -> I.Iteratee B.ByteString IO ()
takeNIterHandle n = I.joinI . I.take n . iterHandle

iterHandle:: Handle -> I.Iteratee B.ByteString IO ()
iterHandle = I.mapChunksM_ . B.hPut


