{-# LANGUAGE TemplateHaskell #-}

module ConcSplit (
       Allocator,
       Impl (..),
       suggestedName,
       concsplit,
       paths2allocators,
       infiniteParts,
       fromPreexistingHandle
    ) where

import System.IO
import Data.List
import Data.Lens.Common
import Data.Lens.Template
import Control.Exception
import Util.Allocator

data Impl = Impl
    {
        _suggestedName :: String,
        _concsplit :: [Allocator Handle] -> Int -> [Allocator Handle] -> IO (),
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

infiniteParts:: String -> [Allocator Handle]
infiniteParts prefix =
    let infiniteNames = map (\n -> prefix ++ show n) [1..]
    in paths2allocators WriteMode infiniteNames

fromPreexistingHandle:: Handle -> Allocator Handle
fromPreexistingHandle h = return (h,return ())

