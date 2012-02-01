{-# LANGUAGE TemplateHaskell #-}

module ConcSplit (
       Impl (..)
    ) where

import System.IO hiding (hGetContents,getContents,readFile,interact)
import Data.List
import Data.Lens.Common
import Data.Lens.Template

data Impl = Impl
    {
        _names :: [String],
        _concSplitFiles :: Int -> [FilePath] -> [(FilePath,Int)] -> IO (),
        _splitHandle :: Int -> Handle -> [(FilePath,Int)] -> IO ()
    }

$( makeLenses [''Impl] )

instance Show Impl where
    show impl = "ConcSplit impl with names " ++  (show $ getL names impl)
