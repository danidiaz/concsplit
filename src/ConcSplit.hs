{-# LANGUAGE TemplateHaskell #-}

module ConcSplit (
       Allocator,
       Impl (..),
       suggestedName,
       concsplit,
       paths2allocators,
       infiniteParts,
       fromPreexistingHandle,
       iterHandle,
       cappedIterHandle
    ) where

import System.IO hiding (hGetContents,getContents,readFile,interact)
import Data.List
import Data.Lens.Common
import Data.Lens.Template
import qualified Data.Iteratee as I
import qualified Data.ByteString as B

type Allocator a = IO (a,IO ())

data Impl = Impl
    {
        _suggestedName :: String,
        _concsplit :: [Allocator Handle] -> [(Allocator Handle,Int)] -> IO (),
        _desc :: String
    }

$( makeLenses [''Impl] )

instance Show Impl where
    show impl = getL desc impl

paths2allocators:: IOMode -> [FilePath] -> [Allocator Handle]
paths2allocators iomode = 
    let path2allocator f = do   
            let openMsg = "opening file \"" ++ f ++ "\" with mode: " ++ show iomode
                closeMsg = "file \"" ++ f ++ "\" closed"  
            putStrLn openMsg 
            handle <- openFile f iomode
            return (handle, putStrLn closeMsg >> hClose handle)   
                  
    in map path2allocator

infiniteParts:: FilePath -> [Int] -> [(Allocator Handle,Int)]
infiniteParts prefix sizes =
    let infiniteSizes = init sizes ++ (repeat $ last sizes)
        infiniteNames = map (\n -> prefix ++ show n) [1..]
    in zip (paths2allocators WriteMode infiniteNames) infiniteSizes 
        

fromPreexistingHandle:: Handle -> Allocator Handle
fromPreexistingHandle h = return (h,return ())


iterHandle:: Handle -> I.Iteratee B.ByteString IO ()
iterHandle = I.mapChunksM_ . B.hPut

cappedIterHandle:: Int -> Handle -> I.Iteratee B.ByteString IO ()
cappedIterHandle n = I.joinI . I.take n . iterHandle
