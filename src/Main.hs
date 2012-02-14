{-# LANGUAGE TemplateHaskell #-}

module Main where

import Prelude hiding (readFile)
import System.IO hiding (hGetContents,getContents,readFile,interact)
--import System.IO.Error
import System.Environment
import System.Console.GetOpt
import Data.Char
import qualified Data.Map as M
import Data.List
import Data.Lens.Common
import Data.Lens.Template
import Data.Default
import Debug.Trace
import Control.Concurrent
import qualified Control.Category as C
import Control.Monad
import Control.Monad.Error
import Control.Applicative
import Control.Exception
import qualified Control.Exception as E
import qualified Data.Text as T
import Data.Text.Read

import Util.Parser
import ConcSplit
import qualified ConcSplit.Leaky as LEAKY

data Conf = Conf
    {
        _filesToJoin :: [FilePath], -- empty list means we read from standard input
        _exitSecDelay :: Int,
        _partPrefix :: FilePath,
        _partSizes :: [Int],
        _impl :: Impl,
        _listMethods :: Bool,
        _helpRequired :: Bool
    } deriving Show

$( makeLenses [''Conf] )

instance Default Conf where
    def = Conf [] 0 "part." [1024] (LEAKY.makeImpl 1024) False False

implMap:: M.Map String Impl
implMap = 
    let addImpl:: M.Map String Impl -> Impl -> M.Map String Impl
        addImpl m impl = M.insert (getL suggestedName impl) impl m
    in foldl' addImpl M.empty [LEAKY.makeImpl 3, LEAKY.makeImpl 1024]

options = [
        let update = \file conf -> pure $ modL filesToJoin  ((:) file) conf 
        in Option ['f'] ["file"] (ReqArg update "filename") "Input file name",

        let update d conf = flip (setL exitSecDelay) conf <$> parseUnadornedInt d
        in Option ['d'] ["delay"] (ReqArg update "seconds") "Delay in seconds before exiting",

        let update m conf = case M.lookup m implMap of 
                Nothing -> throwError $ "Implementation \"" ++ m ++ "\" not found" 
                Just i -> pure $ setL impl i conf
        in Option ['m'] ["method"] (ReqArg update "method") "Method to employ",

        let update = \conf -> pure $ setL listMethods True conf 
        in Option ['l'] ["list"] (NoArg update) "List available methods",

        let update = \conf -> pure $ setL helpRequired True conf 
        in Option ['h'] ["help"] (NoArg update) "Show this help"
    ]

parseNonOpts:: [String] -> Conf -> Either String Conf 
parseNonOpts list conf
      -- if help is requested we don't bother with nonopts
    | getL helpRequired conf = pure conf 
    | getL listMethods conf = pure conf 
    | otherwise = case list of 
            (prefix:sizeList@(s:_)) ->
                let conf' = setL partPrefix prefix conf
                in flip (setL partSizes) conf' <$> mapM parseSize sizeList
            _ -> throwError $ "You need to provide a prefix for the result file," ++ 
                              " and at least one part size!"

printUsage::IO ()
printUsage =
    let header = "concsplit [OPTIONS] PREFIX PARTSIZE [PARTSIZE...]" 
    in putStr $ usageInfo header options

runSelectedImpl :: Conf -> IO ()
runSelectedImpl conf = do
    let parts = infiniteParts (getL partPrefix conf) (getL partSizes conf)   
        files = reverse $ getL filesToJoin conf
        inputs
            |null files = [fromPreexistingHandle stdin]
            |otherwise = paths2allocators ReadMode files
    getL (impl C.>>> concsplit) conf inputs parts

main :: IO ()
main = do 
    args <- getArgs
    let 
        (conftrans,nonopts,errors) = getOpt Permute options args
        errEi
            |null errors = pure ()
            |otherwise = throwError $ head errors 
        --confEi = foldl' (>>=) (pure def) conftrans
        confEi = foldM (flip ($)) def conftrans
        confEi' = errEi *> confEi >>= parseNonOpts nonopts 
    case confEi' of
        Left errmsg -> putStrLn errmsg
        Right conf 
             |getL helpRequired conf -> printUsage
             |getL listMethods conf -> 
                mapM_ putStrLn $ ((map (\(k,v) -> k ++ " - " ++ show v)) . M.assocs) implMap 
             |otherwise -> do 
                let exioHandler =
                        \e -> do
                                putStrLn "An IO exception happened!" 
                                putStrLn $ show (e::E.IOException) 
                                hFlush stdout
                E.catch (runSelectedImpl conf) exioHandler
                threadDelay $ (getL exitSecDelay conf)*(1000^2)
