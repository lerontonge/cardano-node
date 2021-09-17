{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PackageImports #-}

module Cardano.Tracer.Handlers
  ( nodeInfoHandler
  , traceObjectsHandler
  ) where

import           Control.Concurrent.Async (forConcurrently_)
import           Control.Concurrent.STM (atomically)
import           Control.Concurrent.STM.TVar (modifyTVar', readTVarIO)
import           Control.Exception (IOException, try)
import "contra-tracer" Control.Tracer (showTracing, stdoutTracer, traceWith)
import           Data.HashMap.Strict ((!), insert)
import           Data.List (nub)

import           Trace.Forward.Protocol.Type (NodeInfo (..))

import           Cardano.Logging (TraceObject)

import           Cardano.Tracer.Configuration
import           Cardano.Tracer.Handlers.Logs.File (writeTraceObjectsToFile)
import           Cardano.Tracer.Handlers.Logs.Journal (writeTraceObjectsToJournal)
import           Cardano.Tracer.Types

-- | Node's info is required for many parts of cardano-tracer.
--   But some of these parts may be inactive (yet) when node's info
--   is accepted, so we have to store it.
nodeInfoHandler
  :: NodeId
  -> AcceptedNodeInfo
  -> NodeInfo
  -> IO ()
nodeInfoHandler nodeId acceptedNodeInfo ni = atomically $
  modifyTVar' acceptedNodeInfo $ insert nodeId ni

traceObjectsHandler
  :: TracerConfig
  -> NodeId
  -> AcceptedNodeInfo
  -> [TraceObject]
  -> IO ()
traceObjectsHandler _ _ _ [] = return ()
traceObjectsHandler TracerConfig{logging} nodeId acceptedNodeInfo traceObjects = do
  -- The protocol guarantees that node's info is received _before_ any trace object(s) from that node.
  -- So if we are here, it means that info about corresponding node is already received and stored.
  nodesInfo <- readTVarIO acceptedNodeInfo
  let NodeInfo{niName} = nodesInfo ! nodeId
  forConcurrently_ (nub logging) $ \LoggingParams{logMode, logRoot, logFormat} ->
    case logMode of
      FileMode ->
        showProblemIfAny $ writeTraceObjectsToFile nodeId niName logRoot logFormat traceObjects
      JournalMode ->
        showProblemIfAny $ writeTraceObjectsToJournal nodeId niName traceObjects
 where
  showProblemIfAny action =
    try action >>= \case
      Left (e :: IOException) -> logTrace $ "cardano-tracer, cannot write trace objects: " <> show e
      Right _ -> return ()

  logTrace = traceWith $ showTracing stdoutTracer