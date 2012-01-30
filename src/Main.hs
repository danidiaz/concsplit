{-# LANGUAGE TemplateHaskell #-}

module Main where

import Prelude hiding (readFile)
import System.IO hiding (hGetContents,getContents,readFile,interact)
import System.IO.Error
import System.Environment
import System.Console.GetOpt
import Data.Char
import Data.Lens.Common
import Data.Lens.Template
import Debug.Trace
import Control.Concurrent
import Control.Monad
import Control.Monad.Error
import Control.Applicative
import qualified Control.Exception as CE
import qualified Data.Text as T
import Data.Text.Read

data Conf = Conf
    {
        _filesToJoin :: [FilePath], -- empty list means we read from standard input
        _chunkSize :: Int,
        _exitSecDelay :: Int,
        _partPrefix :: FilePath,
        _partSizes :: [Int],
        _helpRequired :: Bool
    } deriving Show

$( makeLenses [''Conf] )

defaultConf:: Conf
defaultConf = Conf [] 1024 0 "part." [1024] False

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
        parseMultiplier :: T.Text -> Either String Int
        parseMultiplier sizeDesc 
            | T.null sizeDesc = return 1
            | sizeDesc == kiloMult = return 1024
            | sizeDesc == megaMult = return (1024*1024)
            | sizeDesc == gigaMult = return (1024*1024*1024)
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

        let update = \conf -> pure $ setL helpRequired True conf 
        in Option ['h'] ["help"] (NoArg update) "Show this help"
    ]

parseNonOpts:: [String] -> Conf -> Either String Conf 
parseNonOpts list conf
    | getL helpRequired conf = pure conf
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
        confE = foldM (flip ($)) defaultConf conftrans
        confE' = errE *> confE >>= parseNonOpts nonopts 
    case confE' of
        Left errmsg -> putStrLn errmsg
        Right conf ->
            if (getL helpRequired conf)
            then  
                printUsage
            else 
                putStrLn $ show conf

