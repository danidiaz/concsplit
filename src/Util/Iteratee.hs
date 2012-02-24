{-# LANGUAGE TemplateHaskell #-}

module Util.Iteratee (
       ByteIter,
       iterHandle,
       cappedIterHandle
    ) where

import System.IO
import qualified Data.Iteratee as I
import qualified Data.ByteString as B

type ByteIter = I.Iteratee B.ByteString IO ()

iterHandle:: Handle -> ByteIter
iterHandle = I.mapChunksM_ . B.hPut

cappedIterHandle:: Int -> Handle -> ByteIter
cappedIterHandle n = I.joinI . I.take n . iterHandle


