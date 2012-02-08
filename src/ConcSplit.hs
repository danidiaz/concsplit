{-# LANGUAGE TemplateHaskell #-}

module ConcSplit (
       Allocator,
       Impl (..),
       names,
       run_concsplit,
       paths2allocators,
       infiniteParts,
       fromSingleHandle 
    ) where

import System.IO hiding (hGetContents,getContents,readFile,interact)
import Data.List
import Data.Lens.Common
import Data.Lens.Template

type Allocator a = IO (a,IO ())

data Impl = Impl
    {
        _names :: [String],
        _concsplit :: Int -> [Allocator Handle] -> [(Allocator Handle,Int)] -> IO (),
        _desc :: String
    }

$( makeLenses [''Impl] )

instance Show Impl where
    show impl = (show $ getL names impl) ++ (getL desc impl)

run_concsplit:: Impl -> Int -> [Allocator Handle] -> [(Allocator Handle,Int)] -> IO ()
run_concsplit impl chunks source parts = getL concsplit impl chunks source parts

paths2allocators:: [FilePath] -> [Allocator Handle]
paths2allocators paths = 
    let 
        name2handle = \f -> openFile f WriteMode 
        handle2alloc = \h -> h >>= (\h -> return (h, hClose h))
    in map (handle2alloc . name2handle) paths 

infiniteParts:: FilePath -> [Int] -> [(Allocator Handle,Int)]
infiniteParts prefix sizes =
    let infiniteSizes = init sizes ++ (repeat $ last sizes)
        infiniteNames = map (\n -> prefix ++ show n) [1..]
    in zip (paths2allocators infiniteNames) infiniteSizes 
        

fromSingleHandle:: Handle -> [Allocator Handle]
fromSingleHandle h = [return (h,return ())]

