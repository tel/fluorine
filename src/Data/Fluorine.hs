-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Fluorine
-- Copyright   :  (c) Daniel Haraj 2013
-- License     :  BSD3
-- Maintainer  :  danharaj@gmail.com
-- Stability   :  experimental
-- Portability :  GHC only
--
-- Fluorine is a Functional Reactive Programming library that emphasizes that
-- reactive values are of a coalgebraic nature. It provides a `Moment` monad
-- whose values are actions that take place in an 'instant of computation'.
-- Reactive values can be observed during the course of an instant and have events
-- pushed to them. Pushing an event to a reactive value can cause events to occur
-- recursively. Fluorine has a well defined order of how such recursive events
-- are processed.
-- 
-- A reactive value is essentially defined by a type constructor. If we imagine
-- that time does not exist and reactive values only react to events, then such a
-- value should behave like Cofree f a. If we imagine that reactive values only change
-- with time, then a reactive value should behave like Traced Time a. Fluorine is
-- conceived as a marriage of these two comonads. Reactive values are then constructed
-- coalgebraically. Finally, we lift this idea to live in a monad where reactive values
-- can interact with each other and the real world.
-----------------------------------------------------------------------------
module Data.Fluorine
 ( module Data.Fluorine.Moment
 , module Data.Fluorine.FunSeg
 , module Data.Fluorine.Reactive
 , module Data.Fluorine.Reactive.Source
 , module Data.Fluorine.Reactive.Reagent
 ) where

-- Provides weak references with predictable lifetimes.
import Data.Fluorine.Anchor
-- The Moment monad captures the idea of computation in an 'instant'
import Data.Fluorine.Moment
-- FunSeg (for Function Segment) is a monoid for constructing time-varying values
-- with a better representation than (t -> a).
import Data.Fluorine.FunSeg
-- Reactive is the type of values that change with time and respond to messages.
import Data.Fluorine.Reactive
-- A Source is a Reactive value that responds to no messages.
import Data.Fluorine.Reactive.Source
-- Reagent is a powerful way of defining Reactive values.
import Data.Fluorine.Reactive.Reagent