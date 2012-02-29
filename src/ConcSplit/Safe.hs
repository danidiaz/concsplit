{-# LANGUAGE RankNTypes #-}
module ConcSplit.Safe (
        makeImpl
    ) where 

import System.IO
import qualified Data.ByteString as B
import Control.Monad
import Control.Monad.IO.Class
import Control.Applicative
import Control.Exception
import ConcSplit
import Control.Concurrent
import Data.List
import Util.Parser
import qualified Data.List as L
import qualified Data.Iteratee as I
import Data.Iteratee.IO.Handle
import Data.Iteratee ((><>),(<><))
import Util.Iteratee

makeImpl :: Int -> Impl
makeImpl chunkSize = 
    let 
        prettySize = prettyPrintSize chunkSize
        name = "safe" ++ prettySize
        desc = "iteratee-based, should work in all cases (preferred chunk size of " ++ prettySize ++ ")"
    in Impl name (concsplit_impl chunkSize) desc

combine:: (a -> b -> IO (c, IO () -> IO () -> (IO (),IO()))) -> 
          Allocator a -> Allocator b -> Allocator c 
combine f a1 a2 = do
   let boe = bracketOnError 
   (fin1',fin2',(r,func)) <- boe a1 
                             snd 
                             (\(h1,fin1) -> boe a2 
                                                snd 
                                                (\(h2,fin2) -> fmap ((,,) fin1 fin2) (f h1 h2)))
   let (clean,leak) = func fin1' fin2' 
   clean `onException` leak
   return (r,leak) 

combine_flip f = flip . combine $ (fmap (fmap (fmap (fmap flip)))) (flip f)

concsplit_impl:: Int -> [Allocator Handle] -> Int -> [Allocator Handle] -> IO ()
concsplit_impl chunkSize files2join partSize parts = 
    let mkIter = (=<<) (\(handle,cleanup) -> return (cappedIterHandle partSize handle,cleanup)) 

        chunkSize' = min chunkSize partSize 

        fuse iter iterAllocs = 
            let fuse' (iter',release) = pure (iter >> iter',release)
            in (head iterAllocs >>= fuse') : tail iterAllocs

        gomasked:: (forall a. IO a -> IO a) -> 
                   [Allocator ByteIter] -> -- this list is assumed to be infinite
                   [Allocator Handle] -> 
                   IO () 
        gomasked restore destinations sources = 
            let writeToIter restore handle iter = do 
                    iter' <- restore $ enumHandle chunkSize' handle iter 
                    atEOF <- hIsEOF handle
                    if atEOF then return (Right iter',\a b -> (a,b))
                             else return (Left (handle,iter'),\a b -> (b,a))

                go _ destinations [] = head destinations >>= snd 
                go allocStrategy destinations (source:sources) = do
                    (result,cleanup) <- allocStrategy (writeToIter restore) source (head destinations)
                    case result of 
                        Right iter' -> go combine_flip (pure (iter',cleanup):tail destinations) sources 
                        Left (handle,iter') -> go combine (fuse iter' $ tail destinations) (pure (handle,cleanup):sources)
            in  go combine destinations sources
    in mask $ \restore -> gomasked restore (map mkIter parts) files2join 

