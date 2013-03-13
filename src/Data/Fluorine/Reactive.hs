{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Fluorine.Reactive
-- Copyright   :  (c) Daniel Haraj 2013
-- License     :  BSD3
-- Maintainer  :  danharaj@gmail.com
-- Stability   :  experimental
-- Portability :  GHC only
--
-----------------------------------------------------------------------------
module Data.Fluorine.Reactive where

import Control.Monad.Reader (ReaderT (..))

import Data.Fluorine.Anchor
import Data.Fluorine.Moment

data Reactive t k a where
 Reactive :: forall t k a.
             Anchor
          -> Moment t a
          -> ((forall u. k u -> Moment t (Maybe u)) -> Moment t ())
          -> Reactive t k a
                               
-- Newtypes so that you can store queries and messages to a Reactive value without needing
-- the broken ImpredicativeTypes extension.
newtype Message t k = Message { runMessage :: forall u . k u -> Moment t (Maybe u) }

query :: Reactive t k a -> Moment t a
message :: Reactive t k a -> Message t k -> Moment t ()

{-# INLINE query #-}
{-# INLINE message #-}
query (Reactive a o _) = io (touchAnchor a) >> o
                              
message (Reactive a _ k) (Message m) = io (touchAnchor a) >> k m

-- The standard way of creating a reactive value. It handles cell creation and hook-up.
element :: forall s t k a.
           s
        -> (t -> s -> IO (s, Maybe (Moment t ())))
        -> (s -> Moment t a)
        -> ((forall u. k u -> Moment t (Maybe u)) -> s -> Moment t s)
        -> Moment t (Reactive t k a)
        
-- A reactive value that doesn't have a cell. It's called a compound because it is formed from the
-- interaction of other reactive values.
compound :: Moment t a
         -> ((forall u. k u -> Moment t (Maybe u)) -> Moment t ())
         -> Moment t (Reactive t k a)
         
element s t o k = Moment . ReaderT $ \sc -> do
 c <- mkCell s
 a <- mkCell Nothing
 let push :: (forall u. k u -> Moment t (Maybe u)) -> Moment t ()
     push u = withPushing c (\c -> io (obsCell c) >>= k u >>= io . pushCell c)
     step dt = obsCell c >>= t dt >>= \(c', ma) -> stepCell c c' >> pushCell a ma
 an <- runMoment (register step (finalizeCell c >> obsCell a)) sc
 return (Reactive an (o =<< io (obsCell c)) push)
 
compound o k = io mkAnchor >>= \an -> return (Reactive an o k)

-- Reactive values are sort of like profunctors. They are contravariant in their f parameter
-- and covariant in their a parameter.

dimap :: (Message t g -> Moment t (Message t k)) 
      -> (a -> Moment t b) 
      -> Reactive t k a 
      -> Reactive t g b
lmap :: (Message t g -> Moment t (Message t k)) -> Reactive t k a -> Reactive t g a
rmap :: (a -> Moment t b) -> Reactive t k a -> Reactive t k b

dimap lm rm (Reactive an o k) = Reactive an (rm =<< o) $ \i -> do m <- lm (Message i)
                                                                  k (runMessage m)
lmap lm = dimap lm return
rmap rm = dimap return rm