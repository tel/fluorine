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
type Source s t a = Reactive s t (Const Void) a

-- source is updated by an IO action every frame.
source :: a -> IO a -> Moment s t (Source s t a)
-- creates a callback to an unbounded queue which is emptied each frame.
callback :: ((a -> IO ()) -> IO ()) -> Moment s t (Source s t [a])

source a0 a = element a0 
                      (\_ _ -> (,Nothing) <$> a) 
                      return
                      (const return)
                      
callback k = do
  c <- io newTChanIO
  io (k (atomically . writeTChan c))
  source [] (atomically (get c))
 where get c = tryReadTChan c >>= \case
        Just x -> (x:) <$> get c
        Nothing -> return []