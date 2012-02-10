module ConcSplit.Leaky (
        impl,
        concsplit
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
import qualified Data.List as L
import qualified Data.Iteratee as I
import Data.Iteratee.IO.Handle
import Data.Iteratee ((><>),(<><))

impl:: Impl
impl= Impl ["leaky","vanilla"] concsplit "A leaky method"

-- to do: change concsplit to concEnum, move splitty to impl creation
concsplit:: Int -> [Allocator Handle] -> [(Allocator Handle,Int)] -> IO ()
concsplit chunkSize files2join parts = do
    let splitty = splitterIter parts
        enumy ::[Allocator Handle] -> I.Enumerator B.ByteString IO ()  
        enumy [] ioiter = pure ioiter
        enumy (h:hs) ioiter = do
           resultIter <- CIO.bracket (liftIO h)
                                     (liftIO . snd)
                                     (\(handle,release) -> enumHandle chunkSize handle ioiter)
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
takeNIterHandle n handle = I.joinI $ I.take n $ iterHandle handle

iterHandle:: Handle -> I.Iteratee B.ByteString IO ()
iterHandle handle = I.mapChunksM_ $ B.hPut handle 


