{-# LANGUAGE TemplateHaskell #-}

module Util.Iteratee (
       ByteIter,
       ByteEnum,
       iterHandle,
       cappedIterHandle
    ) where

import System.IO
import qualified Data.Iteratee as I
import qualified Data.ByteString as B

type ByteIter = I.Iteratee B.ByteString IO ()
type ByteEnum a = I.Enumerator B.ByteString IO a 

iterHandle:: Handle -> ByteIter
iterHandle = I.mapChunksM_ . B.hPut

cappedIterHandle:: Int -> Handle -> ByteIter
cappedIterHandle n = I.joinI . I.take n . iterHandle


