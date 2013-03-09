{-# LANGUAGE BangPatterns, LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Fluorine.Moment
-- Copyright   :  (c) Daniel Haraj 2013
-- License     :  BSD3
-- Maintainer  :  danharaj@gmail.com
-- Stability   :  experimental
-- Portability :  GHC only
--
-----------------------------------------------------------------------------
module Data.Fluorine.Moment where

import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Reader (ReaderT (..))
import Control.Monad.Reader.Class
import Control.Monad.IO.Class
import Data.Maybe
import Data.IORef

import Data.Fluorine.Anchor

-- Scheduler is a bookkeeper for handling the passage of time. It holds a list of weak
-- references to pairs of functions for updating values when time passes. The first
-- function in a pair steps a value. The second function finalizes it. All steps are done
-- before all finalizations, and all must be done before the next Moment is processed.
--
-- Finalization steps can emit an action to be processed immediately in the next step.
--
-- The second argument to Scheduler is the action to take in order to determine 
-- the next time step.
data Scheduler t = Scheduler (IORef [APtr (t -> IO (), IO (Maybe (Moment t ())))])
                             (IO t)
-- The Moment monad is simply IO with access to a Scheduler that handles all
-- time-bookkeeping functionality. We wrap it in a newtype to give nicer error messages.
newtype Moment t a = Moment { runMoment :: ReaderT (Scheduler t) IO a }
 deriving ( Functor, Applicative, Monad
          , MonadIO, MonadFix, MonadReader (Scheduler t)
          )
          
mkScheduler :: IO t -> IO (Scheduler t)
stepScheduler :: Scheduler t -> IO ()
-- Register hooks an update action into the Scheduler and returns the Anchor which
-- owns it.
register :: (t -> IO ()) -> IO (Maybe (Moment t ())) -> Moment t Anchor
          
mkScheduler mt = Scheduler <$> newIORef [] <*> return mt
stepScheduler sc@(Scheduler mk tick) = do
 t <- tick
 ms <- readIORef mk
 ms' <- fmap join . forM ms $ \m ->
  deRefAPtr m >>= \case
   Nothing -> return []
   Just (f,s) -> return [((f,s), m)]
 let (fs, ss) = unzip (map fst ms')
 mapM_ ($ t) fs
 ps <- catMaybes <$> sequence ss
 writeIORef mk (map snd ms')
 mapM_ (($ sc) . runReaderT . runMoment) ps

register a1 a2 = Moment . ReaderT $ \(Scheduler m _) -> do
 a <- mkAnchor
 mkAPtr a (a1, a2) >>= \s -> modifyIORef m (s:)
 return a
          
-- The following is inspired by Patai Gergely's elerea package, which uses a similar scheme
-- to update reactive values in that FRP library. elerea is (c) 2009-2012 Patai Gergely and
-- is licensed under the BSD3 license.

-- A reactive value is either Phase or in the middle of Stepping during a time update.
data Phase a = Phase !a | Stepping !a !a
-- A cell is either Cell or in the middle of Pushing during an event occurence. This is how
-- we handle events occuring within events properly.
data Cell t a = Cell (Phase a) | Pushing [MCell t a -> Moment t ()] (Phase a)
type MCell t a = IORef (Cell t a)

{-# INLINABLE mkCell #-}
{-# INLINABLE obsCell#-}
{-# INLINABLE stepCell #-}
{-# INLINABLE pushCell #-}
{-# INLINABLE finalizeCell #-}

mkCell :: a -> IO (MCell t a)
obsCell :: MCell t a -> IO a
stepCell :: MCell t a -> a -> IO ()
pushCell :: MCell t a -> a -> IO ()
finalizeCell :: MCell t a -> IO ()

withPushing :: MCell t a -> (MCell t a -> Moment t ()) -> Moment t ()
exitPushing :: MCell t a -> Moment t ()

mkCell x = newIORef (Cell (Phase x))
obsCell rc = readIORef rc >>= \case
 (Cell p) -> return (obsPhase p)
 (Pushing _ p) -> return (obsPhase p)
stepCell rc y = readIORef rc >>= \case
 (Cell p) -> writeIORef rc (Cell (stepPhase p y))
 _ -> error "Cell was pushing when stepped."
pushCell rc y = readIORef rc >>= \case
 (Cell p) -> writeIORef rc (Cell (pushPhase p y))
 (Pushing fs p) -> writeIORef rc (Pushing fs (pushPhase p y))
finalizeCell rc = readIORef rc >>= \case
 (Cell p) -> writeIORef rc (Cell (finalizePhase p))
 _ -> error "Cell was pushing when finalized."
 
-- The rules of event pushing are as follows: 
-- * No events should occur while time stepping is being processed.
-- * All pushCell occurences should be called by withPushing only.
-- * Inside withPushing, the toplevel event is processed first.
-- |* If it leads to new events for the same Cell, those events are queued up.
-- |* Once an event is done processing, its children are processed in order by exitPush.
-- |* A child event does not know about its siblings. It is processed as if it were toplevel.
--
-- So, if an event s0 is pushed, and it spawns recursive events s1...sn, those will be processed
-- after s0. Any events s0 produces for other Cells happen immediately. Now, if s1 produces children
-- t1..tn, they will be processed before s2...sn, and so forth.
 
-- liftIO is an eyesore
{-# INLINABLE io #-}
io :: IO a -> Moment t a
io = liftIO
 
withPushing rc f = io (readIORef rc) >>= \case
 Cell p -> io (writeIORef rc (Pushing [] p)) >> f rc >> exitPushing rc
 Pushing fs p -> io (writeIORef rc (Pushing (fs++[f]) p))
exitPushing rc = io (readIORef rc) >>= \case
 Cell p -> return ()
 Pushing [] p -> io (writeIORef rc (Cell p))
 Pushing (f:fs) p -> do
  io (writeIORef rc (Pushing [] p))
  f rc
  exitPushing rc
  Cell p' <- io (readIORef rc)
  io (writeIORef rc (Pushing fs p'))
  exitPushing rc
 
{-# INLINABLE obsPhase #-}
{-# INLINABLE stepPhase #-}
{-# INLINABLE pushPhase #-}
{-# INLINABLE finalizePhase #-}
 
obsPhase (Phase x) = x
obsPhase (Stepping x _) = x
stepPhase (Phase x) y = Stepping x y
stepPhase _ _ = error "Cell was stepped twice in a row."
pushPhase (Phase x) y = Phase y
pushPhase _ _ = error "Phase was pushed while being stepped."
finalizePhase (Stepping _ x) = Phase x
finalizePhase _ = error "Phase was finalized without stepping."