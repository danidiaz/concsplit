{-# LANGUAGE TemplateHaskell #-}

module Main where

import Prelude hiding (readFile)
import System.IO hiding (hGetContents,getContents,readFile,interact)
import System.IO.Error
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
import Control.Monad
import Control.Monad.Error
import Control.Applicative
import qualified Control.Exception as CE
import qualified Data.Text as T
import Data.Text.Read

import qualified ConcSplit as CS
import qualified ConcSplit.Leaky as LEAKY

data Conf = Conf
    {
        _filesToJoin :: [FilePath], -- empty list means we read from standard input
        _chunkSize :: Int,
        _exitSecDelay :: Int,
        _partPrefix :: FilePath,
        _partSizes :: [Int],
        _impl :: CS.Impl,
        _listMethods :: Bool,
        _helpRequired :: Bool
    } deriving Show

$( makeLenses [''Conf] )

instance Default Conf where
    def = Conf [] 1024 0 "part." [1024] LEAKY.impl False False

implMap:: M.Map String CS.Impl
implMap = 
    let addImpl:: M.Map String CS.Impl -> CS.Impl -> M.Map String CS.Impl
        addImpl map impl =
            let names = getL CS.names impl
            in foldl' (\m n -> M.insert n impl m) map names 
    in foldl' addImpl M.empty [LEAKY.impl]  

parseIntPrefix:: ((Int,T.Text) -> Either String Int) -> String -> Either String Int
parseIntPrefix postp s =
    let errCtx = flip catchError (\e -> throwError $ "While parsing " ++ s ++ ": " ++ e)
    in errCtx $ postp =<< decimal (T.pack s)

parseInt:: String -> Either String Int
parseInt = 
    let postp (num,rest)
            |T.null rest = return num
            |otherwise = throwError $ "Unexpected characters after " ++ (show num)
    in parseIntPrefix postp

parseSize:: String -> Either String Int
parseSize = 
    let
        kiloMult = T.pack "K"
        megaMult = T.pack "M" 
        gigaMult = T.pack "G" 
        parseMultiplier mult 
            | T.null mult = pure 1
            | mult == kiloMult = pure 1024
            | mult == megaMult = pure (1024^2)
            | mult == gigaMult = pure (1024^3)
            | otherwise = throwError "Unknown size multiplier"
        postp (parsed,rest) = (*) parsed <$> parseMultiplier rest 
    in parseIntPrefix postp

options:: [OptDescr (Conf -> Either String Conf)]
options = [
        let update = \file conf -> pure $ modL filesToJoin  ((:) file) conf 
        in Option ['f'] ["file"] (ReqArg update "filename") "Input file name",

        let update c conf = flip (setL chunkSize) conf <$> parseSize c
        in Option ['c'] ["chunkSize"] (ReqArg update "bytes") "Chunk size in bytes",

        let update d conf = flip (setL exitSecDelay) conf <$> parseInt d
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

main :: IO ()
main = do 
    args <- getArgs
    let 
        (conftrans,nonopts,errors) = getOpt Permute options args
        errE 
            |null errors = pure ()
            |otherwise = throwError $ head errors 
        --confEi = foldl' (>>=) (pure def) conftrans
        confEi = foldM (flip ($)) def conftrans
        confEi' = errE *> confEi >>= parseNonOpts nonopts 
    case confEi' of
        Left errmsg -> putStrLn errmsg
        Right conf 
             |getL helpRequired conf -> printUsage
             |getL listMethods conf -> mapM_ putStrLn $ M.keys implMap 
             |otherwise -> do 
                putStrLn $ show conf
                putStrLn $ show implMap
                threadDelay $ (getL exitSecDelay conf)*(1000^2)
