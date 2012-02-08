module ConcSplit.Leaky (
        impl,
        concsplit
    ) where 

import System.IO
import qualified Data.ByteString as B
import Control.Monad
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

concsplit:: Int -> [Allocator Handle] -> [(Allocator Handle,Int)] -> IO ()
concsplit chunkSize files2join parts = do
    let splitty = splitterIter parts
    return () 
    -- TBD

--enumAllocators:: Int -> [Allocator] -> I.Enumerator B.ByteString IO ()  
--enumAllocators chunkSize allocators = 
-- TBC

--concSplitFiles:: Int -> [FilePath] -> [(FilePath,Int)] -> IO ()
--concSplitFiles chunkSize filesToJoin pieces = do
--    let allFileEnum =  L.map (enumFile chunkSize) filesToJoin
--        globalEnum = L.foldl1' composeEnums allFileEnum 
--    globalEnum $ splitterIter pieces
--    return ()
--
--composeEnums e1 e2 = \i -> e1 i >>= e2
--
--splitHandle:: Int -> Handle -> [(FilePath,Int)] -> IO ()
--splitHandle chunkSize handle pieces = do
--    enumHandle chunkSize handle $ splitterIter pieces
--    return ()

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


