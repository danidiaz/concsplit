{-# LANGUAGE TemplateHaskell #-}

module Util.Iteratee (
       iterHandle,
       cappedIterHandle
    ) where

import System.IO
import qualified Data.Iteratee as I
import qualified Data.ByteString as B

iterHandle:: Handle -> I.Iteratee B.ByteString IO ()
iterHandle = I.mapChunksM_ . B.hPut

cappedIterHandle:: Int -> Handle -> I.Iteratee B.ByteString IO ()
cappedIterHandle n = I.joinI . I.take n . iterHandle
