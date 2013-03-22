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
machine :: Moment s t a -> (a -> Moment s t (f (Moment s t a))) -> Moment s t (Reactive s t f a)
-- The most generalized machine type I could think of.
gmachine :: (Ord t, Num t)
         => FunSeg t (Moment s t a) 
         -> (a -> Moment s t (f (FunSeg t (Moment s t a))))
         -> Moment s t (Reactive s t f a)

animation a = element a
                      (\dt a -> return (stepF dt a, Nothing))
                      (return . extract)
                      (const return)
                       
machine a f = do element a
                         (\_ a -> return (a, Nothing))
                         id
                         (\m a -> (m =<< f =<< a) >>= \case
                          Nothing -> return a
                          Just b -> return b
                         )
                         
gmachine a f = element a
                       (\t a -> return (stepF t a, Nothing))
                       extract
                       (\m a -> (m =<< f =<< extract a) >>= \case
                        Nothing -> return a
                        Just b -> return b
                       )
                                   
                          
-- The previous three constructions pertain to values of a coalgebraic nature.
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

-- References are fairly useful.
data RefK i a = RefK (i -> a)

type RRef s t a = Reactive s t (RefK a) a
makeRef :: a -> Moment s t (RRef s t a)
readRef :: RRef s t a -> Moment s t a
writeRef :: RRef s t a -> a -> Moment s t ()

makeRef i = machine (return i) (const (return (RefK return)))
readRef = query
writeRef r i = message r (Message $ \(RefK f) -> return . Just $ f i)