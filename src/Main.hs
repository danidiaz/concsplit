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

data Options = Options
    {
        _filesToJoin :: [FilePath], -- empty list means we read from standard input
        _chunkSize :: Int,
        _exitSecDelay :: Int,
        _partPrefix :: FilePath,
        _partSizes :: [Int],
        _helpRequired :: Bool
    } deriving Show

$( makeLenses [''Options] )

defaultOptions:: Options
defaultOptions = Options [] 1024 0 "part." [1024] False

changeErr:: String -> Either String a -> Either String a 
changeErr msg x = catchError x (\s2 -> throwError $ msg ++ s2)

parseIntPrefix:: ((Int,T.Text) -> Either String Int) -> String -> Either String Int
parseIntPrefix post s =
    let changeErr x = catchError x (\s2 -> throwError $ "While parsing " ++ s ++ ": " ++ s2)
    in changeErr $ decimal (T.pack s) >>= post

parseInt:: String -> Either String Int
parseInt = 
    let nothingLeft (num,rest)
            |T.null rest = return num
            |otherwise = throwError $ "There are characters after " ++ (show num)
    in parseIntPrefix nothingLeft

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
        parseUnits (parsed,rest) = (*) parsed <$> parseMultiplier rest 
    in parseIntPrefix parseUnits

options :: [OptDescr (Options -> Either String Options)]
options = [
        let update = \f opts -> pure $ modL filesToJoin  ((:) f) opts 
        in Option ['f'] ["file"] (ReqArg update "filename") "Input file name",

        let update f opts = (\n -> setL chunkSize n opts) <$> parseSize f
        in Option ['c'] ["chunkSize"] (ReqArg update "bytes") "Chunk size in bytes",

        let update f opts = (\n -> setL exitSecDelay n opts) <$> parseInt f
        in Option ['d'] ["delay"] (ReqArg update "seconds") "Delay in seconds before exiting",

        let update = \opts -> pure $ setL helpRequired True opts 
        in Option ['h'] ["help"] (NoArg update) "Show this help"
    ]

parseMandatory:: [String] -> Options -> Either String Options 
parseMandatory list opts
    | getL helpRequired opts = pure opts
    | otherwise = case list of 
            (x:sizeList@(s:xs)) ->
                let opts2 = setL partPrefix x opts
                in (\l -> setL partSizes l opts2) <$> mapM parseSize sizeList
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
        (optsEither,nonopts,errors) = getOpt Permute options args
        errorsAsEither 
            |null errors = pure ()
            |otherwise = throwError $ head errors 
        optsz = foldM (flip ($)) defaultOptions optsEither
        mandatory = errorsAsEither *> optsz >>= parseMandatory nonopts 
    case mandatory of
        Left errmsg -> putStrLn errmsg
        Right opts ->
            if (getL helpRequired opts)
            then  
                printUsage
            else 
                putStrLn $ show opts

