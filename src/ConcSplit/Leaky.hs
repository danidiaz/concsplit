module ConcSplit.Leaky (
        impl,
        concSplitFiles,
        splitHandle
    ) where 

import System.IO
import qualified Data.ByteString as B
import Control.Monad
import Control.Monad.IO.Class
import qualified ConcSplit as CS
import Control.Concurrent
import Data.List
import qualified Data.List as L
import qualified Data.Iteratee as I
import Data.Iteratee.IO.Handle
import Data.Iteratee ((><>),(<><))

impl:: CS.Impl
impl= CS.Impl ["leaky","vanilla"] concSplitFiles splitHandle 

concSplitFiles:: Int -> [FilePath] -> [(FilePath,Int)] -> IO ()
concSplitFiles chunkSize filesToJoin pieces = do
    let allFileEnum =  L.map (enumFile chunkSize) filesToJoin
        globalEnum = L.foldl1' composeEnums allFileEnum 
    globalEnum $ splitterIter pieces
    return ()
    --I.run finalIterState

composeEnums e1 e2 = \i -> e1 i >>= e2

splitHandle:: Int -> Handle -> [(FilePath,Int)] -> IO ()
splitHandle chunkSize handle pieces = do
    enumHandle chunkSize handle $ splitterIter pieces
    return ()
    --I.run finalIterState

splitterIter:: [(FilePath,Int)] -> I.Iteratee B.ByteString IO ()
splitterIter [] = return ()
splitterIter ((path,size):xs) = do
    handle <- liftIO $ openFile path WriteMode 
    liftIO $ putStrLn $ show size
    liftIO $ putStrLn $ "created file " ++ path
    liftIO $ threadDelay (1*1000^2)
    takeNIterHandle size handle
    liftIO $ hClose handle    
    splitterIter xs

takeNIterHandle:: Int -> Handle -> I.Iteratee B.ByteString IO ()
takeNIterHandle n handle = I.joinI $ I.take n $ iterHandle handle

iterHandle:: Handle -> I.Iteratee B.ByteString IO ()
iterHandle handle = I.mapChunksM_ $ B.hPut handle 


