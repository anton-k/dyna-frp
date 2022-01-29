module Dyna.Ref(
  IsRef(..),
) where

import Data.IORef
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar

class IsRef ref where
  newRef    :: a -> IO (ref a)
  readRef   :: ref a -> IO a
  writeRef  :: ref a -> a -> IO ()
  modifyRef :: ref a -> (a -> a) -> IO ()

instance IsRef IORef where
  newRef = newIORef
  readRef = readIORef
  writeRef = writeIORef
  modifyRef = modifyIORef'

instance IsRef TVar where
  newRef = newTVarIO
  readRef = readTVarIO
  writeRef ref v = atomically $ writeTVar ref v
  modifyRef ref f = atomically $ modifyTVar' ref f

