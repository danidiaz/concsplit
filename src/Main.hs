{-# LANGUAGE TemplateHaskell #-}

module Main where

import Prelude hiding (catch,(.))
import System.IO
import System.Environment
import System.Console.GetOpt
import Data.Char
import qualified Data.Map as M
import Data.List
import Data.Lens.Common
import Data.Lens.Template
import Data.Default
import Control.Category
import Control.Monad
import Control.Monad.Error
import Control.Applicative
import Control.Exception

import Util.Parser
import Util.Allocator
import ConcSplit
import qualified ConcSplit.Leaky as LEAKY
import qualified ConcSplit.Safe as SAFE

data Conf = Conf
    {
        _filesToJoin :: [FilePath], -- empty list means we read from standard input
        _partPrefix :: FilePath,
        _partSizes :: Int,
        _impl :: Impl,
        _listMethods :: Bool,
        _helpRequired :: Bool
    } deriving Show

$( makeLenses [''Conf] )

instance Default Conf where
    def = Conf [] "__part." (1024^2) (SAFE.makeImpl 1024) False False

implMap:: M.Map String Impl
implMap = 
    let addImpl:: M.Map String Impl -> Impl -> M.Map String Impl
        addImpl m impl = M.insert (getL suggestedName impl) impl m
    in foldl' addImpl M.empty [ LEAKY.makeImpl 3, LEAKY.makeImpl 1024,
                                SAFE.makeImpl 3, SAFE.makeImpl 1024
                              ]

options = [
        let update p conf = pure $ setL partPrefix p conf
        in Option ['p'] ["prefix"] (ReqArg update "prefix") "Filename prefix for the output part files",

        let update s conf = (\p -> setL partSizes p conf) <$> parseSize s
        in Option ['s'] ["size"] (ReqArg update "size") ("Output parts file size\n" ++ 
                                                         "(can be expressed in the following units: b,K,M,G)\n" ++
                                                         "(default: "++ (prettyPrintSize $ getL partSizes def) ++")"),
        let update m conf = case M.lookup m implMap of 
                Nothing -> throwError $ "Implementation \"" ++ m ++ "\" not found" 
                Just i -> pure $ setL impl i conf
        in Option ['m'] ["method"] (ReqArg update "method") $ "Method to employ\n(default: "++ getL (impl >>> suggestedName) def ++")",

        let update = \conf -> pure $ setL listMethods True conf 
        in Option ['l'] ["list"] (NoArg update) "List available methods",

        let update = \conf -> pure $ setL helpRequired True conf 
        in Option ['h'] ["help"] (NoArg update) "Show this help"
    ]

printUsage::IO ()
printUsage = do
    let header = "concsplit [OPTIONS] [FILE...]" 
        examples = [ "concsplit input1.txt -s 1M -m safe1K",
                     "concsplit input1.txt input2.txt -p output -s 1K8b",
                     "concsplit -p readfromstdin -s 1K",
                     "concsplit -l",
                     "concsplit -h"
                   ]
    putStr $ usageInfo header options
    putStrLn "Examples:" 
    mapM_ putStrLn . map ((++) "\t") $ examples

runSelectedImpl :: Conf -> IO ()
runSelectedImpl conf = do
    let parts = infiniteParts (getL partPrefix conf) 
        files = getL filesToJoin conf
        inputs
            |null files = [fromPreexistingHandle stdin]
            |otherwise = paths2allocators ReadMode files
    getL (impl >>> concsplit) conf inputs (getL partSizes conf) parts

main :: IO ()
main = do 
    args <- getArgs
    let 
        (conftrans,nonopts,errors) = getOpt Permute options args
        errEi
            |null errors = pure ()
            |otherwise = throwError $ head errors 
        confEi = liftA (setL filesToJoin nonopts) (foldM (flip ($)) def conftrans)
    case errEi *> confEi of
        Left errmsg -> putStrLn errmsg
        Right conf 
             |getL helpRequired conf -> printUsage
             |getL listMethods conf -> 
                mapM_ putStrLn $ ((map (\(k,v) -> k ++ " - " ++ show v)) . M.assocs) implMap 
             |otherwise -> do 
                putStrLn . show $ conf
                let exioHandler =
                        \e -> do
                                putStrLn "An IO exception happened!" 
                                putStrLn $ show (e::IOException) 
                                hFlush stdout
                catch (runSelectedImpl conf) exioHandler
