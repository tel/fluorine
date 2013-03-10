{-# LANGUAGE RankNTypes #-}
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

-- Think of Reactive t f a as Cofree f a, except you cannot peak ahead:
-- you can only examine the a you have and the continuation of the value
-- that is encoded by the type constructor f. You can also choose a continuation
-- in order to advance the value. The time dependency of Reactive is implicit.
data Reactive t f a = Reactive Anchor
                               (forall r. Query f a r -> Moment t r)
                               (Message f a -> Moment t ())

-- Newtypes so that you can store queries and messages to a Reactive value without needing
-- the broken ImpredicativeTypes extension.                  
newtype Query f a r = Query { runQuery :: forall u. a -> f u -> r }
newtype Message f a = Message { runMessage :: forall t u. a -> f u -> Moment t (Maybe u) }

query :: Reactive t f a -> Query f a r -> Moment t r
message :: Reactive t f a -> Message f a -> Moment t ()

{-# INLINE query #-}
{-# INLINE message #-}
query (Reactive _ k _) q = k q
message (Reactive _ _ k) m = k m

-- The standard way of creating a reactive value. It handles cell creation and hook-up.
reactive :: s
         -> (t -> s -> IO (s, Maybe (Moment t ())))
         -> (forall r. Query f a r -> s -> Moment t r)
         -> (Message f a -> s -> Moment t s)
         -> Moment t (Reactive t f a)

reactive s t o f = Moment . ReaderT $ \sc -> do
 c <- mkCell s
 a <- mkCell Nothing
 let push u = withPushing c (\c -> io (obsCell c) >>= f u >>= io . pushCell c)
     step dt = obsCell c >>= t dt >>= \(c', ma) -> stepCell c c' >> pushCell a ma
 an <- runMoment (register step (finalizeCell c >> obsCell a)) sc
 return (Reactive an (\q -> o q =<< io (obsCell c)) push)