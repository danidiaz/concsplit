{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module Util.Parser (
        parseUnadornedInt,
        parseSize,
        prettyPrintSize
    ) where

import Control.Monad
import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Error
import Control.Monad.Error.Class
import Data.List
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Text.Read

errCtx:: MonadError String m => String -> m a -> m a
errCtx ctx = flip catchError (\e -> throwError $ ctx ++ ": " ++ e)

parseUnadornedInt:: String -> Either String Int 
parseUnadornedInt str = 
        let checkRest (d,rest) = if T.null rest
                then pure d
                else throwError $ "unexpected \"" ++ show rest ++ "\""
            result = (decimal . T.pack) str >>= checkRest 
        in  errCtx ("while parsing integer \"" ++ str ++ "\"") result

parse:: [(T.Text,Int)] -> String -> Either String Int 
parse multipliers = parseDecimal 0 . T.pack
    where
        parseDecimal accum text
            | T.null text = pure accum
            | otherwise = 
                 let parseMult::(Int,T.Text) -> [(T.Text,Int)] -> Either String Int
                     parseMult (_,text') [] = 
                        throwError $ "unexpected " ++ show text'
                     parseMult partial@(notyetmult,text') ((multText,multValue):mults) =
                        case T.stripPrefix multText text' of
                            Nothing -> parseMult partial mults 
                            Just text'' -> parseDecimal (accum + notyetmult*multValue) text''
                  in decimal text >>= flip parseMult multipliers 

prettyPrint:: [(T.Text,Int)] -> Int -> String
prettyPrint multipliers number =
    let prettyPrint' _ text [] = T.unpack text
        prettyPrint' amount text ((multText,mult):ms) =
            if amount >= mult
            then let text' = foldl' T.append T.empty [text, T.pack . show $ amount `div` mult, multText]
                 in prettyPrint' (amount `mod` mult) text' ms 
            else prettyPrint' amount text ms 
    in prettyPrint' number T.empty multipliers 
 
sizeMults :: [(T.Text,Int)]
sizeMults = reverse $ zip (map T.pack ["b","K","M","G"]) (map (1024^) [0..])

parseSize :: String -> Either String Int 
parseSize text = errCtx ("while parsing size " ++ text) (parse sizeMults text)

prettyPrintSize::Int -> String
prettyPrintSize = prettyPrint sizeMults 
