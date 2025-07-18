{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -Wno-partial-fields -Wno-orphans #-}

{- HLINT ignore "Redundant <$>" -}
{- HLINT ignore "Move filter" -}

module Cardano.Unlog.LogObject
  ( HostLogs (..)
  , TraceFreqs
  , hlRawLogObjects
  , hlTraceFreqs
  , RunLogs (..)
  , rlLogs
  , LogObject (..)
  , loPretty
  --
  , logObjectStreamInterpreterKeysLegacy
  , logObjectStreamInterpreterKeys
  , LOBody (..)
  , LOAnyType (..)
  , fromTextRef
  , textRefEquals
  )
where

import           Cardano.Analysis.API.Ground
import           Cardano.Logging.Resources.Types
import           Cardano.Prelude hiding (Text, show, toText)
import           Cardano.Util

import           Prelude (show, unzip3)

import qualified Data.Aeson as AE
import qualified Data.Aeson.Key as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import           Data.Aeson.Types (Parser)
import           Data.Data (Data)
import           Data.Hashable (hash)
import qualified Data.Map.Lazy as ML (Map)
import qualified Data.Map.Strict as Map
import           Data.Profile
import           Data.String (IsString (..))
import qualified Data.Text as LText
import           Data.Text.Short (ShortText, fromText, toText)
import qualified Data.Text.Short as Text
import           Data.Tuple.Extra (fst3, snd3, thd3)
import           Data.Vector (Vector)
import qualified Data.Vector as V


type Text       = ShortText

type TraceFreqs = ML.Map Text Int


-- | Us of the a TextRef replaces commonly expected string parses with references
--   into a Map, reducing memory footprint - given that large runs can contain
--   >25mio log objects.
data TextRef
    = TextRef {-# UNPACK #-} !Int
    | TextLit {-# UNPACK #-} !Text
  deriving Generic
  deriving anyclass NFData

toTextRef :: Text -> TextRef
toTextRef t = let h = hash t in if Text.null (lookupTextRef h) then TextLit t else TextRef h

fromTextRef :: TextRef -> Text
fromTextRef (TextRef i) = lookupTextRef i
fromTextRef (TextLit t) = t

textRefEquals :: Text -> TextRef -> Bool
textRefEquals t = (t ==) . fromTextRef

instance Show TextRef where
  show (TextRef i) = show $ lookupTextRef i
  show (TextLit t) = show t

instance IsString TextRef where
  fromString = toTextRef . fromString

instance ToJSON TextRef where
  toJSON (TextRef i) = toJSON $ lookupTextRef i
  toJSON (TextLit t) = toJSON t

-- | Input data.
data HostLogs a
  = HostLogs
    { hlRawLogfiles    :: [FilePath]
    , hlRawLines       :: Int
    , hlRawTraceFreqs  :: TraceFreqs
    , hlLogs           :: (LogObjectSource, a)
    , hlProfile        :: [ProfileEntry I]
    , hlRawFirstAt     :: Maybe UTCTime
    , hlRawLastAt      :: Maybe UTCTime
    }
  deriving (Generic, Functor, NFData)

deriving instance FromJSON a => FromJSON (HostLogs a)
deriving instance   ToJSON a =>   ToJSON (HostLogs a)

hlRawLogObjects :: HostLogs a -> Int
hlRawLogObjects = sum . Map.elems . hlRawTraceFreqs

hlTraceFreqs :: HostLogs a -> HostLogs TraceFreqs
hlTraceFreqs HostLogs{hlLogs = (source, _), ..} =
  HostLogs {hlLogs = (source, hlRawTraceFreqs), ..}

data RunLogs a
  = RunLogs
    { rlHostLogs      :: Map.Map Host (HostLogs a)
    , rlFilterDate    :: UTCTime
    }
  deriving (Generic, Functor, FromJSON, ToJSON, NFData)

rlLogs :: RunLogs a -> [(LogObjectSource, a)]
rlLogs = fmap hlLogs . Map.elems . rlHostLogs

data LogObject
  = LogObject
    { loAt   :: !UTCTime
    , loNS   :: !TextRef
    , loKind :: !TextRef
    , loHost :: !Host
    , loTid  :: !TId
    , loBody :: !LOBody
    }
  deriving (Generic, Show)
  deriving anyclass NFData

instance ToJSON LogObject

deriving instance NFData a => NFData (Resources a)


loPretty :: LogObject -> LText.Text
loPretty LogObject{..} = mconcat
  [ stripS . LText.pack $ show loAt, " "
  , LText.pack $ show loBody ]
 where stripS x = fromMaybe x $ LText.stripSuffix " UTC" x

--
-- Compat wrappers:
--
--- Needed becayse Ouroboros.Network.Block.BlockNo(..) imports a newtype instance,
---  ..whereas node's logs might contain an { unBlockNo :: BlockNo } object.
newtype BlockNoCompat =
  BlockNoCompat { unBlockNo :: BlockNo }
  deriving stock Generic
  deriving anyclass FromJSON

--
-- LogObject stream interpretation
--
type Threeple t = (t, t, t)

interpreters :: Threeple (Map Text (Object -> Parser LOBody))
interpreters = map3ple Map.fromList . unzip3 . fmap ent $
  -- Every second:
  [ (,,,) "Resources" "Resources" "Resources" $
    \v -> LOResources <$> parsePartialResourceStates (Object v)

  -- Leadership:
  , (,,,) "TraceStartLeadershipCheck" "Forge.Loop.StartLeadershipCheckPlus" "Forge.Loop.StartLeadershipCheck" $
    \v -> LOTraceStartLeadershipCheck
            <$> v .: "slot"
            <*> (v .:? "utxoSize"     <&> fromMaybe 0)
            <*> (v .:? "chainDensity" <&> fromMaybe 0)

  , (,,,) "TraceBlockContext" "Forge.BlockContext" "Forge.Loop.BlockContext" $
    \v -> LOBlockContext
            <$> v .: "current slot"
            <*> ((v .: "tipBlockNo")
                 -- BlockContext's block number is inconsistent
                 -- with the rest of traces.
                 <&> BlockNo . fromIntegral . pred @Int)

  , (,,,) "TraceLedgerState" "Forge.LedgerState" "Forge.Loop.LedgerState" $
    \v -> LOLedgerState
            <$> v .: "slot"

  , (,,,) "TraceLedgerView" "Forge.LedgerView" "Forge.Loop.LedgerView" $
    \v -> LOLedgerView
            <$> v .: "slot"

  , (,,,) "TraceNodeIsLeader" "Forge.NodeIsLeader" "Forge.Loop.NodeIsLeader" $
    \v -> LOTraceLeadershipDecided
            <$> v .: "slot"
            <*> pure True

  , (,,,) "TraceNodeNotLeader" "Forge.NodeNotLeader" "Forge.Loop.NodeNotLeader" $
    \v -> LOTraceLeadershipDecided
            <$> v .: "slot"
            <*> pure False

  , (,,,) "TraceForgeTickedLedgerState" "Forge.TickedLedgerState" "Forge.Loop.TickedLedgerState" $
    \v -> LOTickedLedgerState
            <$> v .: "slot"

  , (,,,) "TraceForgingMempoolSnapshot" "Forge.MempoolSnapshot" "Forge.Loop.MempoolSnapshot" $
    \v -> LOMempoolSnapshot
            <$> v .: "slot"

  -- Forging:
  , (,,,) "TraceForgedBlock" "Forge.ForgedBlock" "Forge.Loop.ForgedBlock" $
    \v -> LOBlockForged
            <$> v .: "slot"
            <*> v .: "blockNo"
            <*> v .: "block"
            <*> v .: "blockPrev"

  -- Receipt:
  , (,,,) "ChainSyncClientEvent.TraceDownloadedHeader" "ChainSyncClient.ChainSyncClientEvent.DownloadedHeader" "ChainSync.Client.DownloadedHeader" $
    \v -> LOChainSyncClientSeenHeader
            <$> v .: "slot"
            <*> ((v .: "blockNo")
                 <|>
                 ((v .: "blockNo") <&>
                  \(BlockNoCompat x) -> x))
            <*> v .: "block"

  , (,,,) "SendFetchRequest" "BlockFetchClient.SendFetchRequest" "BlockFetch.Client.SendFetchRequest" $
    \v -> LOBlockFetchClientRequested
            <$> v .: "head"
            <*> v .: "length"

  , (,,,) "CompletedBlockFetch" "BlockFetchClient.CompletedBlockFetch" "BlockFetch.Client.CompletedBlockFetch" $
    \v -> LOBlockFetchClientCompletedFetch
            <$> v .: "block"

  -- Forwarding:
  , (,,,) "ChainSyncServerEvent.TraceChainSyncServerRead.AddBlock" "unknown0" "unknown1" $
    \v -> LOChainSyncServerSendHeader . fromMaybe (error $ "Incompatible LOChainSyncServerSendHeader: " <> show v)
          <$>  v .:? "block"

  , (,,,) "ChainSyncServerEvent.TraceChainSyncServerReadBlocked.AddBlock" "ChainSyncServerEvent.TraceChainSyncServerUpdate" "ChainSync.ServerHeader.Update" $
    \v -> case ( KeyMap.lookup "risingEdge" v
               , KeyMap.lookup "blockingRead" v
               , KeyMap.lookup "rollBackTo" v) of
            (Just (Bool False), _, _) -> pure $ LOAny LAFallingEdge v
            (_, Just (Bool False), _) -> pure $ LOAny LANonBlocking v
            (_, _, Just _)            -> pure $ LOAny LARollback    v
            -- Should be either rising edge+rollforward, or legacy:
            _ -> do
              blockLegacy <- v .:? "block"
              block       <- v .:? "addBlock"
              pure $
                LOChainSyncServerSendHeader
                ((block <|> blockLegacy)
                  & fromMaybe (error $ "Incompatible LOChainSyncServerSendHeader: " <> show v)
                  & Text.take 64
                  & Hash)

  , (,,,) "TraceBlockFetchServerSendBlock" "BlockFetchServer.SendBlock" "BlockFetch.Server.SendBlock" $
    \v -> LOBlockFetchServerSending
            <$> v .: "block"

  -- Adoption:
  , (,,,) "TraceAddBlockEvent.AddedToCurrentChain" "ChainDB.AddBlockEvent.AddedToCurrentChain" "ChainDB.AddBlockEvent.AddedToCurrentChain" $
    \v -> LOBlockAddedToCurrentChain
            <$> ((v .: "newtip")     <&> hashFromPoint)
            <*> pure SNothing
            <*> (v .:? "chainLengthDelta"
                -- Compat for node versions 1.27 and older:
                 <&> fromMaybe 1)

  , (,,,) "TraceAdoptedBlock" "Forge.AdoptedBlock" "Forge.Loop.AdoptedBlock" $
    \v -> LOBlockAddedToCurrentChain
            <$> v .: "blockHash"
            <*> ((v .: "blockSize") <&> SJust)
            <*> pure 1

  -- Ledger related:
  , (,,,) "TraceSnapshotEvent.TookSnapshot" "TraceLedgerEvent.TookSnapshot" "ChainDB.LedgerEvent.TookSnapshot" $
    \_ -> pure LOLedgerTookSnapshot
  -- If needed, this could track slot and duration (SMaybe):
  -- {"at":"2024-10-19T10:16:27.459112022Z","ns":"ChainDB.LedgerEvent.TookSnapshot","data":{"enclosedTime":{"tag":"RisingEdge"},"kind":"TookSnapshot","snapshot":{"kind":"snapshot"},"tip":"RealPoint (SlotNo 5319) adefbb19d6284aa68f902d33018face42d37e1a7970415d2a81bd4c2dea585ba"},"sev":"Info","thread":"81","host":"client-us-04"}
  -- {"at":"2024-10-19T10:16:45.925381225Z","ns":"ChainDB.LedgerEvent.TookSnapshot","data":{"enclosedTime":{"contents":18.466253914,"tag":"FallingEdgeWith"},"kind":"TookSnapshot","snapshot":{"kind":"snapshot"},"tip":"RealPoint (SlotNo 5319) adefbb19d6284aa68f902d33018face42d37e1a7970415d2a81bd4c2dea585ba"},"sev":"Info","thread":"81","host":"client-us-04"}

  , (,,,) "LedgerMetrics" "LedgerMetrics" "LedgerMetrics" $
    \v -> LOLedgerMetrics
            <$> v .: "slot"
            <*> v .: "utxoSize"
            <*> v .: "chainDensity"

  -- Tx receive path & mempool:
  , (,,,) "TraceBenchTxSubServAck" "TraceBenchTxSubServAck" "TraceBenchTxSubServAck" $
    \v -> LOTxsAcked <$> v .: "txIds"

  , (,,,) "TraceTxSubmissionCollected" "TraceTxSubmissionCollected" "TxSubmission.TxInbound.Collected" $
    \v -> LOTxsCollected
            <$> v .: "count"

  , (,,,) "TraceTxSubmissionProcessed" "TraceTxSubmissionProcessed" "TxSubmission.TxInbound.Processed" $
    \v -> LOTxsProcessed
            <$> v .: "accepted"
            <*> v .: "rejected"

  , (,,,) "TraceMempoolAddedTx" "Mempool.AddedTx" "Mempool.AddedTx" $
    \v -> do
      x :: Object <- v .: "mempoolSize"
      LOMempoolTxs <$> x .: "numTxs"

  , (,,,) "TraceMempoolRemoveTxs" "Mempool.RemoveTxs" "Mempool.RemoveTxs" $
    \v -> do
      x :: Object <- v .: "mempoolSize"
      LOMempoolTxs <$> x .: "numTxs"

  , (,,,) "TraceMempoolRejectedTx" "Mempool.RejectedTx" "Mempool.RejectedTx" $
    \_ -> pure LOMempoolRejectedTx

  -- Generator:
  , (,,,) "TraceBenchTxSubSummary" "TraceBenchTxSubSummary" "Benchmark.BenchTxSubSummary" $
    \v -> do
       x :: Object <- v .: "summary"
       LOGeneratorSummary
         <$> ((x .: "ssFailures" :: Parser [Text])
              <&> null)
         <*> x .: "ssTxSent"
         <*> x .: "ssElapsed"
         <*> x .: "ssThreadwiseTps"
  ]
 where
   hashFromPoint :: LText.Text -> Hash
   hashFromPoint = Hash . fromText . LText.take 64

   ent :: (a,b,c,d) -> ((a,d), (b,d), (c, d))
   ent (a,b,c,d) = ((a,d), (b,d), (c, d))

   map3ple :: (a -> b) -> (a,a,a) -> (b,b,b)
   map3ple f (x,y,z) = (f x, f y, f z)



logObjectStreamInterpreterKeysLegacy, logObjectStreamInterpreterKeys :: [Text]
logObjectStreamInterpreterKeysLegacy =
  logObjectStreamInterpreterKeysLegacy1 <> logObjectStreamInterpreterKeysLegacy2
 where
   logObjectStreamInterpreterKeysLegacy1 = Map.keys (interpreters & fst3)
   logObjectStreamInterpreterKeysLegacy2 = Map.keys (interpreters & snd3)
logObjectStreamInterpreterKeys       = Map.keys (interpreters & thd3)

data LOBody
  -- Every second:
  = LOResources !ResourceStats
  -- Leadership:
  | LOTraceStartLeadershipCheck !SlotNo !Word64 !Double
  | LOBlockContext
    { loSlotNo           :: !SlotNo
    , loBlockNo          :: !BlockNo
    }
  | LOLedgerState
    { loSlotNo           :: !SlotNo
    }
  | LOLedgerView
    { loSlotNo           :: !SlotNo
    }
  | LOTraceLeadershipDecided
    { loSlotNo           :: !SlotNo
    , loLeader           :: !Bool
    }
  | LOTickedLedgerState
    { loSlotNo           :: !SlotNo
    }
  | LOMempoolSnapshot
    { loSlotNo           :: !SlotNo
    }
  -- Forging:
  | LOBlockForged
    { loSlotNo           :: !SlotNo
    , loBlockNo          :: !BlockNo
    , loBlock            :: !Hash
    , loPrev             :: !Hash
    }
  -- Receipt:
  | LOChainSyncClientSeenHeader
    { loSlotNo           :: !SlotNo
    , loBlockNo          :: !BlockNo
    , loBlock            :: !Hash
    }
  | LOBlockFetchClientRequested
    { loBlock            :: !Hash
    , loLength           :: !Int
    }
  | LOBlockFetchClientCompletedFetch
    { loBlock            :: !Hash
    }
  -- Forwarding:
  | LOChainSyncServerSendHeader
    { loBlock            :: !Hash
    }
  | LOBlockFetchServerSending
    { loBlock            :: !Hash
    }
  -- Adoption:
  | LOBlockAddedToCurrentChain
    { loBlock            :: !Hash
    , loSize             :: !(SMaybe Int)
    , loLength           :: !Int
    }
  -- Ledger related:
  | LOLedgerTookSnapshot
  | LOLedgerMetrics
    { loSlotNo           :: !SlotNo
    , loUtxoSize         :: !Word64
    , loChainDensity     :: !Double
    }
  -- Tx receive path & mempool:
  | LOTxsAcked !(Vector Text)
  | LOTxsCollected !Word64
  | LOTxsProcessed !Word64 !Int
  | LOMempoolTxs !Word64
  | LOMempoolRejectedTx
  -- Generator:
  | LOGeneratorSummary !Bool !Word64 !NominalDiffTime ![Double]
  -- Everything else:
  | LOAny !LOAnyType !Object
  | LODecodeError
    { loRawText :: !ShortText
    , loError   :: !ShortText
    }
  deriving (Eq, Generic, Show, Data)
  deriving anyclass NFData

data LOAnyType
  = LAFallingEdge
  | LANonBlocking
  | LARollback
  | LANoInterpreter
  deriving (Eq, Generic, NFData, Read, Show, ToJSON, Data)

deriving instance Eq       ResourceStats
deriving instance Data     ResourceStats

instance ToJSON LOBody

instance FromJSON LogObject where
  parseJSON = AE.withObject "LogObject" $ \v -> do
    body :: Object <- v .: "data"
    -- XXX:  fix node causing the need for this workaround
    (,) unwrapped kind <- unwrap "credentials" "val" body
    nsVorNs :: Value <- v .: "ns"
    let ns = case nsVorNs of
               Array (V.toList -> [String ns']) -> fromText ns'
               String ns' -> fromText ns'
               x -> error $
                 "The 'ns' field must be either a string, or a singleton-String vector, was: " <> show x
    LogObject
      <$> v .: "at"
      <*> pure (toTextRef ns)
      <*> pure (toTextRef kind)
      <*> v .: "host"
      <*> v .: "thread"
      <*> case Map.lookup  ns                                       (thd3 interpreters)
           <|> Map.lookup  ns                                       (snd3 interpreters)
           <|> Map.lookup (kind
                           & Text.stripPrefix "Cardano.Node."
                           & fromMaybe kind)                        (snd3 interpreters)
           <|> Map.lookup  kind                                     (fst3 interpreters) of
            Just interp -> interp unwrapped
            Nothing -> pure $ LOAny LANoInterpreter v
   where
     unwrap :: Text -> Text -> Object -> Parser (Object, Text)
     unwrap wrappedKeyPred unwrapKey v = do
       kind <- (fromText <$>) <$> v .:? "kind"
       wrapped   :: Maybe Text <-
         (fromText <$>) <$> v .:? Aeson.fromText (toText wrappedKeyPred)
       unwrapped :: Maybe Object <- v .:? Aeson.fromText (toText unwrapKey)
       case (kind, wrapped, unwrapped) of
         (Nothing, Just _, Just x) -> (,) <$> pure x <*> (fromText <$> x .: "kind")
         (Just kind0, _, _) -> pure (v, kind0)
         _ -> pure (v, "")

parsePartialResourceStates :: Value -> Parser (Resources Word64)
parsePartialResourceStates =
  AE.withObject "NodeSetup" $
    \o -> do
      rCentiCpu   <- o .:  "CentiCpu"
      rCentiGC    <- o .:  "CentiGC"
      rCentiMut   <- o .:  "CentiMut"
      rGcsMajor   <- o .:  "GcsMajor"
      rGcsMinor   <- o .:  "GcsMinor"
      rAlloc      <- o .:  "Alloc"
      rLive       <- o .:  "Live"
      rHeap       <- o .:? "Heap"       .!= 0
      rRSS        <- o .:  "RSS"
      rCentiBlkIO <- o .:  "CentiBlkIO"
      rNetRd      <- o .:? "NetRd"      .!= 0
      rNetWr      <- o .:? "NetWr"      .!= 0
      rFsRd       <- o .:? "FsRd"       .!= 0
      rFsWr       <- o .:? "FsWr"       .!= 0
      rThreads    <- o .:  "Threads"
      pure Resources{..}

{-# NOINLINE lookupTextRef #-}
lookupTextRef :: Int -> Text
lookupTextRef ref = Map.findWithDefault Text.empty ref dict
  where
    dict    = Map.fromList [(hash t, t) | t <- concat [allKeys, kinds, legacy, newTr]]
    kinds   = map ("Cardano.Node." <>) allKeys
    allKeys = concatMap Map.keys [fst3 interpreters, snd3 interpreters, thd3 interpreters]
              & filter (not . Text.null)

    -- common string parses from legacy tracing with no known interpreter
    legacy = map ("cardano.node." <>)
      [ "BlockFetchClient"
      , "BlockFetchServer"
      , "ChainDB"
      , "ChainSyncClient"
      , "ChainSyncHeaderServer"
      , "DnsSubscription"
      , "Forge"
      , "IpSubscription"
      , "LeadershipCheck"
      , "Mempool"
      , "resources"
      , "TxInbound"
      ]

    -- common string parses from new tracing with no known interpreter
    newTr =
      [ "AcknowledgedFetchRequest"
      , "AddedFetchRequest"
      , "BlockFetch.Client.AcknowledgedFetchRequest"
      , "BlockFetch.Client.AddedFetchRequest"
      , "BlockFetch.Client.CompletedFetchBatch"
      , "BlockFetch.Client.StartedFetchBatch"
      , "BlockFetch.Remote.Receive.BatchDone"
      , "BlockFetch.Remote.Receive.Block"
      , "BlockFetch.Remote.Receive.StartBatch"
      , "BlockFetchServer"
      , "ChainDB.AddBlockEvent.AddBlockValidation.UpdateLedgerDb"
      , "ChainDB.AddBlockEvent.AddBlockValidation.ValidCandidate"
      , "ChainDB.AddBlockEvent.AddedBlockToQueue"
      , "ChainDB.AddBlockEvent.AddedBlockToVolatileDB"
      , "ChainDB.AddBlockEvent.ChangingSelection"
      , "ChainDB.AddBlockEvent.IgnoreBlockAlreadyInVolatileDB"
      , "ChainDB.AddBlockEvent.PipeliningEvent.OutdatedTentativeHeader"
      , "ChainDB.AddBlockEvent.PipeliningEvent.SetTentativeHeader"
      , "ChainDB.AddBlockEvent.PoppedBlockFromQueue"
      , "ChainDB.AddBlockEvent.TryAddToCurrentChain"
      , "ChainDB.CopyToImmutableDBEvent.CopiedBlockToImmutableDB"
      , "ChainDB.FollowerEvent.NewFollower"
      , "ChainDB.GCEvent.ScheduledGC"
      , "ChainDB.IteratorEvent.StreamFromVolatileDB"
      , "ChainSyncServer.Update"
      , "CompletedFetchBatch"
      , "CopiedBlockToImmutableDB"
      , "DownloadedHeader"
      , "Forge.ForgingStats"
      , "ForgingStats"
      , "IgnoreBlockAlreadyInVolatileDB"
      , "Net.Handshake.Local.Receive.ProposeVersions"
      , "Net.Handshake.Local.Send.AcceptVersion"
      , "OutdatedTentativeHeader"
      , "Recv"
      , "ResourceStats"
      , "SetTentativeHeader"
      , "StartedFetchBatch"
      , "StateQueryServer.Receive.Query"
      , "StateQueryServer.Receive.Release"
      , "StreamFromVolatileDB"
      , "TraceAddBlockEvent.ChangingSelection"
      , "TraceAddBlockEvent.PoppedBlockFromQueue"
      , "TraceTxInboundCanRequestMoreTxs"
      , "TraceTxInboundCannotRequestMoreTxs"
      , "TxSubmission.TxInbound.CanRequestMoreTxs"
      , "TxSubmission.TxInbound.CannotRequestMoreTxs"
      , "UpdateLedgerDbTraceEvent.StartedPushingBlockToTheLedgerDb"
      ]
