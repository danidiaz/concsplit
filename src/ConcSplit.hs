{-# LANGUAGE TemplateHaskell #-}

module ConcSplit (
       Allocator,
       AllocStrategy,
       Impl (..),
       suggestedName,
       concsplit,
       paths2allocators,
       infiniteParts,
       fromPreexistingHandle,
       allocLeftToRight,
       allocRightToLeft
    ) where

import System.IO hiding (hGetContents,getContents,readFile,interact)
import Data.List
import Data.Lens.Common
import Data.Lens.Template
import Control.Monad.CatchIO as CIO

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
                closeMsg = "closing file \"" ++ f ++ "\""  
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

type BiAllocator a b = IO (a,IO (), b, IO ())

type AllocStrategy a b = Allocator a -> Allocator b -> BiAllocator a b

allocLeftToRight::AllocStrategy a b
allocLeftToRight a1 a2 =
    CIO.bracketOnError a1
                       snd
                       (\(handle,release) -> a2 >>= \(handle2,release2) -> return (handle,release,handle2,release2))

allocRightToLeft::AllocStrategy a b
allocRightToLeft a1 a2 = allocLeftToRight a2 a1 >>= \(x,xCleanup,y,yCleanup) -> return (y,yCleanup,x,xCleanup)

