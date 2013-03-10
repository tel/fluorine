{-# LANGUAGE TupleSections, LambdaCase #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Fluorine.Reactive.Source
-- Copyright   :  (c) Daniel Haraj 2013
-- License     :  BSD3
-- Maintainer  :  danharaj@gmail.com
-- Stability   :  experimental
-- Portability :  GHC Only
--
-----------------------------------------------------------------------------
module Data.Fluorine.Reactive.Source where

import Control.Applicative
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import Data.Void

import Data.Fluorine.Moment
import Data.Fluorine.Reactive

-- Sources don't respond to any messages.
type Source t a = Reactive t (Const Void) a

-- source is updated by an IO action every frame.
source :: a -> IO a -> Moment t (Source t a)
-- callback attaches a callback to an unbounded queue which is emptied each frame.
callback :: ((a -> IO ()) -> IO ()) -> Moment t (Source t [a])

source a0 a = reactive a0 
                       (\_ _ -> (,Nothing) <$> a) 
                       (\(Query q) x -> return (q x undefined))
                       (const return)
                       
callback k = do
  c <- io newTChanIO
  io (k (atomically . writeTChan c))
  source [] (atomically (get c))
 where get c = tryReadTChan c >>= \case
        Just x -> (x:) <$> get c
        Nothing -> return []