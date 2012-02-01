{-# LANGUAGE TemplateHaskell #-}

module ConcSplit (
       Impl (..),
       names, concSplitFiles, splitHandle,
       infiniteParts
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
    show impl = "ConcSplit impl " ++  (show $ getL names impl)


infiniteParts:: FilePath -> [Int] -> [(FilePath,Int)]
infiniteParts prefix sizes =
    let infiniteSizes = init sizes ++ (repeat $ last sizes)
        infiniteNames = map (\n -> prefix ++ show n) [1..]
    in zip infiniteNames infiniteSizes 
        
