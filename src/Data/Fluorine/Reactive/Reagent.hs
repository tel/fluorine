{-# LANGUAGE TupleSections, LambdaCase #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Fluorine.Reactive.Reagent
-- Copyright   :  (c) Daniel Haraj 2013
-- License     :  BSD3
-- Maintainer  :  danharaj@gmail.com
-- Stability   :  experimental
-- Portability :  GHC only
--
-----------------------------------------------------------------------------
module Data.Fluorine.Reactive.Reagent where

import Control.Comonad

import Data.Fluorine.FunSeg
import Data.Fluorine.Moment
import Data.Fluorine.Reactive
import Data.Fluorine.Reactive.Source

-- If we want to define a Reactive value that does not receive messages
animation :: (Ord t, Num t) => FunSeg t a -> Moment s t (Source s t a)
-- If we want to define a timeless Reactive value that receives messages
machine :: a -> (a -> Moment s t (f a)) -> Moment s t (Reactive s t f a)

animation a = element a
                      (\dt a -> return (stepF dt a, Nothing))
                      (return . extract)
                      (const return)
                       
machine a f = do k <- f a
                 element (a, k)
                         (\_ a -> return (a, Nothing))
                         (return . fst)
                         (\m (a,k) -> m k >>= \case
                          Nothing -> return (a, k)
                          Just b -> (b,) <$> f b
                         )
                          
-- The previous two constructions pertain to values of a coalgebraic nature.
-- The first, FunSeg, a comonad of time. The second, a coalgebra of f, or equivalently
-- one of Cofree f, although we add a layer of Moment t for dynamic effects.

-- A fully general Reactive value changes with time and reacts to messages at the same time.
-- In particular, we want something coalgebraic with respect to the "product of comonads"
-- of FunSeg and Cofree f. Furthermore, we want to add appropriate monadic layers
-- to allow full access to dynamic effects. The result is the following, rather ugly construction.

-- Our method of mixing comonads and effects.
data Mix s t f g a = a :<< f (g (Moment s t (Mix s t f g a)))
infixl 3 :<<

-- "A time-varying effect of the environment that produces a value, and a continuation that
-- has access to effects and produces a new reagent."
type Reagent s t f a = FunSeg t (Moment s t (Mix s t f (FunSeg t) a))

reagent :: (Ord t, Num t) => Reagent s t f a -> Moment s t (Reactive s t f a)
reagent r = element r
                    (\dt a -> return (stepF dt a, Nothing))
                    (\r -> do 
                     (x :<< _) <- extract r
                     return x
                    )
                    (\m r -> do
                     (_ :<< k) <- extract r
                     m k >>= \case
                      Nothing -> return r
                      Just b -> return b
                    )
