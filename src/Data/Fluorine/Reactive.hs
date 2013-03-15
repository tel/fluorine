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

data Reactive s t k a where
 Reactive :: forall s t k a.
             Anchor
          -> Moment s t a
          -> ((forall u. k u -> Moment s t (Maybe u)) -> Moment s t ())
          -> Reactive s t k a
                               
-- Newtypes so that you can store queries and messages to a Reactive value without needing
-- the broken ImpredicativeTypes extension.
newtype Message s t k = Message { runMessage :: forall u . k u -> Moment s t (Maybe u) }

query :: Reactive s t k a -> Moment s t a
message :: Reactive s t k a -> Message s t k -> Moment s t ()

{-# INLINE query #-}
{-# INLINE message #-}
query (Reactive a o _) = io (touchAnchor a) >> o
                              
message (Reactive a _ k) (Message m) = io (touchAnchor a) >> k m

-- The standard way of creating a reactive value. It handles cell creation and hook-up.
element :: forall r s t k a.
           s
        -> (t -> s -> IO (s, Maybe (Moment r t ())))
        -> (s -> Moment r t a)
        -> ((forall u. k u -> Moment r t (Maybe u)) -> s -> Moment r t s)
        -> Moment r t (Reactive r t k a)
        
-- A reactive value that doesn't have a cell. It's called a compound because it is formed from the
-- interaction of other reactive values.
compound :: Moment s t a
         -> ((forall u. k u -> Moment s t (Maybe u)) -> Moment s t ())
         -> Moment s t (Reactive s t k a)
         
element s t o k = Moment . ReaderT $ \sc -> do
 c <- mkCell s
 a <- mkCell Nothing
 let push :: (forall u. k u -> Moment r t (Maybe u)) -> Moment r t ()
     push u = withPushing c (\c -> io (obsCell c) >>= k u >>= io . pushCell c)
     step dt = obsCell c >>= t dt >>= \(c', ma) -> stepCell c c' >> pushCell a ma
 an <- runMoment (register step (finalizeCell c >> obsCell a)) sc
 return (Reactive an (o =<< io (obsCell c)) push)
 
compound o k = io mkAnchor >>= \an -> return (Reactive an o k)

-- Reactive values are sort of like profunctors. They are contravariant in their f parameter
-- and covariant in their a parameter.

dimap :: (Message s t g -> Moment s t (Message s t k)) 
      -> (a -> Moment s t b) 
      -> Reactive s t k a 
      -> Reactive s t g b
lmap :: (Message s t g -> Moment s t (Message s t k)) -> Reactive s t k a -> Reactive s t g a
rmap :: (a -> Moment s t b) -> Reactive s t k a -> Reactive s t k b

dimap lm rm (Reactive an o k) = Reactive an (rm =<< o) $ \i -> do m <- lm (Message i)
                                                                  k (runMessage m)
lmap lm = dimap lm return
rmap rm = dimap return rm