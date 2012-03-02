{-# LANGUAGE TemplateHaskell #-}

module Util.Allocator (
        Allocator,
        combine,
        combine_flipped    
    ) where

import Control.Arrow (first,second)
import Control.Exception

type Release = IO ()

type Allocator a = IO (a,Release)

combine:: (a -> b -> IO (c, Release -> Release -> (Release,Release))) -> 
          Allocator a -> Allocator b -> Allocator c 
combine f a1 a2 = do
   let boe = bracketOnError 
   (fin1',fin2',(r,combineFins)) <- boe a1 
                                    snd 
                                    (\(h1,fin1) -> boe a2 
                                                   snd 
                                                   (\(h2,fin2) -> fmap ((,,) fin1 fin2) (f h1 h2)))
   let (closeNow,closeLater) = combineFins fin1' fin2' 
   closeNow `onException` closeLater
   return (r,closeLater) 

-- Lets you play with the order of allocation. 
-- Here, the second allocator parameter is allocated *first*.
combine_flipped f = flip . combine $ (fmap (fmap (fmap (second flip)))) (flip f)

