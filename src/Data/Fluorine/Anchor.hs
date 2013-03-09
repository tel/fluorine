{-# LANGUAGE MagicHash, UnboxedTuples #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Fluorine.Anchor
-- Copyright   :  (c) Daniel Haraj 2013
-- License     :  BSD3
-- Maintainer  :  danharaj@gmail.com
-- Stability   :  experimental
-- Portability :  GHC only
--
-- This module provides weak references under the name of APtr's whose
-- lives are attached to values of type Anchor. An Anchor is a value with
-- semantics similar to a ForeignPtr's in that they can be guaranteed to be
-- alive by calling `touchAnchor`. This makes APtr's far more resilient to
-- compiler optimizations as compared to raw GHC.Weak pointers, which can have
-- completely unpredictable semantics depending on which -Olevel is used while
-- compiling!
-----------------------------------------------------------------------------
module Data.Fluorine.Anchor where

import Control.Applicative
import GHC.ForeignPtr
import GHC.Prim (mkWeak#)
import GHC.Types (IO(..))
import GHC.Weak

newtype Anchor = Anchor (ForeignPtr Int) deriving (Eq, Ord, Show)
newtype APtr a = APtr (Weak a)

mkAnchor :: IO Anchor
mkAPtr :: Anchor -> a -> IO (APtr a)
deRefAPtr :: APtr a -> IO (Maybe a)
touchAnchor :: Anchor -> IO ()
finalizeAnchor :: Anchor -> IO ()
finalizeAPtr :: APtr a -> IO ()

mkAnchor = Anchor <$> mallocForeignPtr
mkAPtr (Anchor f@(ForeignPtr a# h)) x = do 
  w <- IO $ \s -> case mkWeak# h x (return () :: IO ()) s of
                   (# s1, w #) -> (# s1, Weak w #)
  return (APtr w)
deRefAPtr (APtr w) = deRefWeak w
touchAnchor (Anchor f) = touchForeignPtr f
finalizeAnchor (Anchor f) = finalizeForeignPtr f
finalizeAPtr (APtr w) = finalize w