-- |
-- =
--
-- This module is just like Prometheus.Registry, except that it does not use a
-- global TVar, so it's possible to have more than one registry in the same
-- executable.
--
-- The interface exposed is mostly STM, which gives the user finer-grained
-- control of the registry.
module Prometheus.Registry.Local (
    Registry
  , newRegistry
  , newRegistryIO
  , register
  , registerIO
  , collect
  , STM.atomically
  ) where

import Prometheus.Metric

import Control.Monad.IO.Class
import Control.Concurrent.STM (STM, TVar)
import qualified Control.Concurrent.STM as STM

-- | A 'Registry' is a list of all registered metrics, currently represented by
-- their sampling functions.
type Registry = TVar [IO [SampleGroup]]

-- | Create a new registry. There is no need to close it explicity, it's left
-- to the garbage collector. To avoid memory leaks, it's sensible to treat it
-- like a proper resource and use a bracket pattern where it's not used outside
-- of the continuation.
newRegistry :: STM Registry
newRegistry = STM.newTVar []

-- | 'newRegistry' but in IO.
newRegistryIO :: IO Registry
newRegistryIO = STM.newTVarIO []

-- | Register a metric. The metric cannot be un-registered.
register :: Registry -> IO [SampleGroup] -> STM ()
register rg sampleGroupsIO = STM.modifyTVar' rg (sampleGroupsIO:)

-- | Like 'register' but the metrics is constructed in IO and then the register
-- STM transaction is executed.
registerIO :: MonadIO m => Registry -> Metric s -> m s
registerIO rg (Metric mk) = liftIO (mk >>= \(s, sampleGroupsIO) -> do
  _ <- STM.atomically (register rg sampleGroupsIO)
  pure s)

-- | Collect samples from all currently registered metrics. In typical use cases
-- there is no reason to use this function, instead you should use
-- `exportLocalMetricsAsText` or a convenience library.
--
-- This function is likely only of interest if you wish to export metrics in
-- a non-supported format for use with another monitoring service.
collect :: MonadIO m => Registry -> m [SampleGroup]
collect rg = liftIO $ do
    registry <- STM.atomically $ STM.readTVar rg
    concat <$> sequence registry
