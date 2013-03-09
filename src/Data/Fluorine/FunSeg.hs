{-# LANGUAGE BangPatterns #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Fluorine.FunSeg
-- Copyright   :  (c) Daniel Haraj 2013
-- License     :  BSD3
-- Maintainer  :  danharaj@gmail.com
-- Stability   :  Experimental
-- Portability :  GHC Only
--
-----------------------------------------------------------------------------
module Data.Fluorine.FunSeg where

import Control.Applicative
import Control.Comonad
import Data.Semigroup

data FunSeg t a = F !t [(t,(t->a))]

{-# INLINABLE stepF #-}
{-# INLINABLE funseg #-}
{-# INLINABLE funseg' #-}
{-# INLINABLE cutF #-}
{-# INLINABLE reparF #-}
{-# INLINABLE splitF #-}
stepF :: (Ord t, Num t) => t -> FunSeg t a -> FunSeg t a
funseg :: (Ord t, Num t) => t -> (t -> a) -> FunSeg t a
cutF :: (Ord t, Num t) => t -> FunSeg t a -> FunSeg t a
-- Reparametrize time. Parametrization must be *STRICTLY* monotonic and increasing.
reparF :: (Ord t, Num t) => (t -> t) -> FunSeg t a -> FunSeg t a
splitF :: (Ord t, Num t) => FunSeg t (a,b) -> (FunSeg t a, FunSeg t b)

stepF dt (F t []) = F (t+dt) []
stepF dt (F t ((s,f):fs)) = 
 case s >= 0 of
  True -> case t+dt >= s of
   False -> F (t+dt) ((s,f):fs)
   True -> stepF (s-t-dt) (F 0 fs)
  False -> F (t+dt) [(-1,f)]
 
funseg s f = F 0 [(s, f)]
funseg' f = F 0 [(-1, f)]

cutF s (F t0 fs) = F t0 (work s t0 fs)
 where work _ _ [] = []
       work s t0 ((t,f):fs) = case t < 0 of
        True -> [(s+t0,f)]
        False -> case compare (t-t0) s of
         LT -> (t,f) : work (s-t+t0) 0 fs
         GT -> [(s+t0, f)]
         EQ -> [(t, f)]
         
reparF s (F t0 fs) = F (s t0) (aux 0 fs)
 where aux _ [] = []
       aux t0 ((t,f):fs) = (s (t+t0) - s t0, f.s) : aux (t+t0) fs
 
splitF b = (fmap fst b, fmap snd b)
 
instance Functor (FunSeg t) where
 {-# INLINABLE fmap #-}
 fmap f (F t0 gs) = F t0 (map (\(s,g) -> (s, f.g)) gs)

instance (Ord t, Num t) => Comonad (FunSeg t) where
 {-# INLINABLE extract #-}
 {-# INLINABLE duplicate #-}
 extract (F t0 ((_,f):_)) = f t0
 extract (F t0 []) = error "Extracted undefined behavior."
 duplicate (F t0 fs) =
  let aux t0 ((t,f):fs) = case t < 0 of
                           True -> [(-1, (\t -> F (t0+t) [(-1,f)]))]
                           False -> (t-t0, (\t -> F (t0+t) ((t,f):fs))) : aux 0 fs
      aux t0 [] = [(-1, \t -> F (t+t0) [])]
  in F 0 (aux t0 fs)
 
instance (Ord t, Num t) => Applicative (FunSeg t) where
 {-# INLINABLE pure #-}
 pure x = F 0 [(-1, const x)]
 {-# INLINABLE (<*>) #-}
 (F u0 fs) <*> (F v0 xs) = 
  let aux !u0 !v0 ((!u,f):fs) ((!v,x):xs) = 
       case u < 0 of
        True -> case v < 0 of
         True -> [(-1, \t -> (f (t+u0)) (x (t+v0)))]
         False -> (v-v0, \t -> (f (t+u0)) (x (t+v0))) : aux (u0+v-v0) 0 [(-1,f)] xs
        False -> case v < 0 of
         True -> (u-u0, \t -> (f (t+u0)) (x (t+v0))) : aux 0 (v0+u-u0) fs [(-1,x)]
         False -> case compare (u-u0) (v-v0) of
          LT -> (u-u0, \t -> (f (t+u0)) (x (t+v0))) : aux 0 (v0+u-u0) fs ((v,x):xs)
          GT -> (v-v0, \t -> (f (t+u0)) (x (t+v0))) : aux (u0+v-v0) 0 ((u,f):fs) xs
          EQ -> (u-u0, \t -> (f (t+u0)) (x (t+v0))) : aux 0 0 fs xs
  in F 0 (aux u0 v0 fs xs)
  
instance (Ord t, Num t) => ComonadApply (FunSeg t) where
 {-# INLINE (<@>) #-}
 (<@>) = (<*>)
  
instance Num t => Semigroup (FunSeg t a) where
 {-# INLINABLE (<>) #-}
 (F t0 fs) <> ~(F s0 ((s,g):gs)) = F t0 $ fs ++ (s-s0, \t -> g (t+s0)):gs