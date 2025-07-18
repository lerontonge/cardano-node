{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{- HLINT ignore "Redundant id" -}

module Cardano.Testnet.Test.Cli.LeadershipSchedule
  ( hprop_leadershipSchedule
  ) where

import           Cardano.Api
import qualified Cardano.Api as Api

import           Cardano.Node.Configuration.Topology
import           Cardano.Testnet

import           Prelude

import           Control.Monad (void)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson as J
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.Aeson.Types as J
import           Data.Default.Class
import           Data.List ((\\))
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Time.Clock as DTC
import qualified GHC.Stack as GHC
import           System.FilePath ((</>))
import qualified System.Info as SYS

import           Testnet.Components.Configuration
import           Testnet.Components.Query
import           Testnet.Process.Cli.Keys
import           Testnet.Process.Cli.SPO
import           Testnet.Process.Run (execCli, execCli', mkExecConfig)
import           Testnet.Property.Assert
import           Testnet.Property.Util (decodeEraUTxO, integrationRetryWorkspace)
import           Testnet.Runtime
import           Testnet.Types

import           Hedgehog (Property, (===))
import qualified Hedgehog as H
import qualified Hedgehog.Extras.Stock.IO.Network.Port as H
import qualified Hedgehog.Extras.Stock.IO.Network.Sprocket as IO
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H
import qualified Hedgehog.Extras.Test.TestWatchdog as H

-- | Execute me with:
-- @DISABLE_RETRIES=1 cabal test cardano-testnet-test --test-options '-p "/leadership-schedule/"'@
hprop_leadershipSchedule :: Property
hprop_leadershipSchedule = integrationRetryWorkspace 2 "leadership-schedule" $ \tempAbsBasePath' -> H.runWithDefaultWatchdog_ $ do
  H.note_ SYS.os
  conf@Conf { tempAbsPath=tempAbsPath@(TmpAbsolutePath work) } <- mkConf tempAbsBasePath'
  let tempBaseAbsPath = makeTmpBaseAbsPath tempAbsPath
      ceo = ConwayEraOnwardsConway
      sbe = convert ceo
      asbe = AnyShelleyBasedEra sbe
      cTestnetOptions = def
        { cardanoNodeEra = asbe
        , cardanoNodes =
          [ SpoNodeOptions []
          , SpoNodeOptions []
          , SpoNodeOptions []
          ]
        }
      eraString = eraToString sbe

  tr@TestnetRuntime
    { testnetMagic
    , wallets=wallet0:_
    , configurationFile
    , testnetNodes
    } <- createAndRunTestnet cTestnetOptions def conf

  node1sprocket <- H.headM $ testnetSprockets tr
  execConfig <- mkExecConfig tempBaseAbsPath node1sprocket testnetMagic

  ----------------Need to register an SPO------------------
  let utxoAddr = Text.unpack $ paymentKeyInfoAddr wallet0
      utxoSKeyFile = signingKey $ paymentKeyInfoPair wallet0
  void $ execCli' execConfig
    [ eraString, "query", "utxo"
    , "--address", utxoAddr
    , "--cardano-mode"
    , "--out-file", work </> "utxo-1.json"
    ]

  utxo1Json <- H.leftFailM . H.readJsonFile $ work </> "utxo-1.json"
  UTxO utxo1 <- H.noteShowM $ decodeEraUTxO sbe utxo1Json
  txin1 <- H.noteShow =<< H.headM (Map.keys utxo1)
  let node1SocketPath = Api.File $ IO.sprocketSystemName node1sprocket
      termEpoch = EpochNo 15
  epochStateView <- getEpochStateView configurationFile node1SocketPath
  keyDeposit <- getKeyDeposit epochStateView ceo
  (stakePoolIdNewSpo, KeyPair{signingKey=File stakePoolColdSigningKey, verificationKey=File stakePoolColdVKey}, KeyPair{signingKey=File vrfSkey})
    <- registerSingleSpo asbe 1 tempAbsPath
         configurationFile
         node1SocketPath
         (EpochNo 10)
         testnetMagic
         keyDeposit
         execConfig
         (txin1, utxoSKeyFile, utxoAddr)

  -- Create test stake address to delegate to the new stake pool
  -- NB: We need to fund the payment credential of the overall address
  --------------------------------------------------------------

  let testStakeDelegator = work </> "test-delegator"

  H.createDirectoryIfMissing_ testStakeDelegator
  let testDelegatorKeys = KeyPair
        { signingKey = File $ testStakeDelegator </> "test-delegator.skey"
        , verificationKey = File $ testStakeDelegator </> "test-delegator.vkey"
        }
      testDelegatorPaymentKeys = KeyPair
        { signingKey = File $ testStakeDelegator </> "test-delegator-payment.skey"
        , verificationKey = File $ testStakeDelegator </> "test-delegator-payment.vkey"
        }
      testDelegatorRegCertFp = testStakeDelegator </> "test-delegator.regcert"
      testDelegatorDelegCert = testStakeDelegator </> "test-delegator.delegcert"

  cliStakeAddressKeyGen testDelegatorKeys
  cliAddressKeyGen testDelegatorPaymentKeys

  -- NB: We must include the stake credential
  testDelegatorPaymentAddr <- execCli
                [ "latest", "address", "build"
                , "--testnet-magic", show @Int testnetMagic
                , "--payment-verification-key-file", verificationKeyFp testDelegatorPaymentKeys
                , "--stake-verification-key-file", verificationKeyFp testDelegatorKeys
                ]
  testDelegatorStakeAddress
    <- filter (/= '\n')
         <$> execCli
               [ "latest", "stake-address", "build"
               , "--stake-verification-key-file", verificationKeyFp testDelegatorKeys
               , "--testnet-magic", show @Int testnetMagic
               ]

  -- Test stake address registration cert
  createStakeKeyRegistrationCertificate
    tempAbsPath
    (cardanoNodeEra cTestnetOptions)
    (verificationKey testDelegatorKeys)
    keyDeposit
    testDelegatorRegCertFp

  -- Test stake address deleg  cert
  createStakeDelegationCertificate
    tempAbsPath
    sbe
    (verificationKey testDelegatorKeys)
    stakePoolIdNewSpo
    testDelegatorDelegCert

  -- TODO: Refactor getting valid UTxOs into a function
  H.note_  "Get updated UTxO"

  void $ execCli' execConfig
      [ "latest", "query", "utxo"
      , "--address", utxoAddr
      , "--cardano-mode"
      , "--out-file", work </> "utxo-2.json"
      ]

  H.cat $ work </> "utxo-2.json"

  utxo2Json <- H.leftFailM . H.readJsonFile $ work </> "utxo-2.json"
  UTxO utxo2 <- H.noteShowM $ decodeEraUTxO sbe utxo2Json
  txin2 <- H.noteShow =<< H.headM (Map.keys utxo2)

  let delegRegTestDelegatorTxBodyFp = work </> "deleg-register-test-delegator.txbody"

  void $ execCli' execConfig
    [ eraString
    , "transaction", "build"
    , "--change-address", testDelegatorPaymentAddr -- NB: A large balance ends up at our test delegator's address
    , "--tx-in", Text.unpack $ renderTxIn txin2
    , "--tx-out", utxoAddr <> "+" <> show @Int 5_000_000
    , "--witness-override", show @Int 3
    , "--certificate-file", testDelegatorRegCertFp
    , "--certificate-file", testDelegatorDelegCert
    , "--out-file", delegRegTestDelegatorTxBodyFp
    ]

  let delegRegTestDelegatorTxFp = work </> "deleg-register-test-delegator.tx"
  void $ execCli
    [ "latest", "transaction", "sign"
    , "--tx-body-file", delegRegTestDelegatorTxBodyFp
    , "--testnet-magic", show @Int testnetMagic
    , "--signing-key-file", unFile utxoSKeyFile
    , "--signing-key-file", signingKeyFp testDelegatorKeys
    , "--out-file", delegRegTestDelegatorTxFp
    ]

  H.note_ "Submitting test delegator registration and delegation certificates..."

  void $ execCli' execConfig
           [ "latest", "transaction", "submit"
           , "--tx-file", delegRegTestDelegatorTxFp
           ]

  -------------------------------------------------------------------

  let testDelegatorStakeAddressInfoOutFp = work </> "test-delegator-stake-address-info.json"
  void $ checkStakeKeyRegistered
           tempAbsPath
           configurationFile
           node1SocketPath
           termEpoch
           execConfig
           testDelegatorStakeAddress
           testDelegatorStakeAddressInfoOutFp

  -- TODO: We need a separate function that allows us to run single nodes after
  -- we have started a cluster with create-staked
  let testSpoDir = work </> "test-spo"
      topologyFile = testSpoDir </> "topology.json"
  H.createDirectoryIfMissing_ testSpoDir
  let valency = 1
      topology = RealNodeTopology $
        flip map testnetNodes $ \TestnetNode{nodeIpv4,nodePort} ->
          RemoteAddress (showIpv4Address nodeIpv4) nodePort valency
  H.lbsWriteFile topologyFile $ Aeson.encode topology
  let testSpoKesVKey = work </> "kes.vkey"
      testSpoKesSKey = work </> "kes.skey"

  cliNodeKeyGenKes
    $ KeyPair (File testSpoKesVKey) (File testSpoKesSKey)
  let testSpoOperationalCertFp = testSpoDir </> "node-operational.cert"

  void $ execCli' execConfig
    [ "latest", "node", "new-counter"
    , "--cold-verification-key-file", stakePoolColdVKey
    , "--counter-value", "0"
    , "--operational-certificate-issue-counter-file", testSpoOperationalCertFp
    ]


  void $ execCli' execConfig
      [ "latest", "node", "issue-op-cert"
      , "--kes-period", "0"
      , "--kes-verification-key-file", testSpoKesVKey
      , "--cold-signing-key-file", stakePoolColdSigningKey
      , "--operational-certificate-issue-counter-file", testSpoOperationalCertFp
      , "--out-file", testSpoOperationalCertFp
      ]

  jsonBS <- Aeson.encodePretty . Aeson.Object <$> createConfigJson tempAbsPath sbe
  H.lbsWriteFile (unFile configurationFile) jsonBS
  newNodePort <- H.randomPort testnetDefaultIpv4Address
  eRuntime <- runExceptT . retryOnAddressInUseError $
    startNode (TmpAbsolutePath work) "test-spo" testnetDefaultIpv4Address newNodePort testnetMagic
        [ "run"
        , "--config", unFile configurationFile
        , "--topology", topologyFile
        , "--database-path", testSpoDir </> "db"
        , "--shelley-kes-key", testSpoKesSKey
        , "--shelley-vrf-key", vrfSkey
        , "--shelley-operational-certificate", testSpoOperationalCertFp
        ]
  testPoolStdOutFp <- case eRuntime of
                       Left e -> H.failMessage GHC.callStack $ "Failed to start node: " <> show e
                       Right runtime -> return $ nodeStdout runtime

  -- Wait for 2 epochs to pass
  void $ waitUntilEpoch configurationFile
                        node1SocketPath (EpochNo 3)

  currentLeaderShipScheduleFile <- H.noteTempFile work "current-schedule.log"

  void $ execCli' execConfig
    [ "latest", "query", "leadership-schedule"
    , "--genesis", shelleyGenesisFile tr
    , "--stake-pool-id", stakePoolIdNewSpo
    , "--vrf-signing-key-file", vrfSkey
    , "--out-file", currentLeaderShipScheduleFile
    , "--current"
    ]

  currentScheduleJson <- H.leftFailM $ H.readJsonFile currentLeaderShipScheduleFile

  expectedLeadershipSlotNumbers <- H.noteShowM $ fmap (fmap slotNumber) $ H.leftFail $ J.parseEither (J.parseJSON @[LeadershipSlot]) currentScheduleJson

  maxSlotExpected <- H.noteShow $ maximum expectedLeadershipSlotNumbers

  H.assert $ not (L.null expectedLeadershipSlotNumbers)

  leadershipDeadline <- H.noteShowM $ DTC.addUTCTime 90 <$> H.noteShowIO DTC.getCurrentTime

    -- We need enough time to pass such that the expected leadership slots generated by the
    -- leadership-schedule command have actually occurred.
    -- TODO: We can further improve this if parameterize foldEpochState's callback on
    -- the current slot and current block number.
  (leaderSlots, notLeaderSlots) <- H.byDeadlineM 10 leadershipDeadline "Wait for chain to surpass all expected leadership slots" $ do
    (someLeaderSlots, someNotLeaderSlots) <- getRelevantSlots testPoolStdOutFp (minimum expectedLeadershipSlotNumbers)
    if L.null someLeaderSlots
      then H.failure
      else do
        maxActualSlot <- H.noteShow $ maximum someLeaderSlots
        H.assert $ maxActualSlot >= maxSlotExpected
        pure (someLeaderSlots, someNotLeaderSlots)

  H.noteShow_ expectedLeadershipSlotNumbers
  H.noteShow_ leaderSlots
  H.noteShow_ notLeaderSlots

    -- Double check that we've seen all slots
  H.noteShow_ ("Slots not seen as TraceNodeIsLeader nor TraceNodeNotLeader" :: Text)
  ([minimum expectedLeadershipSlotNumbers .. maxSlotExpected] \\ leaderSlots) \\ notLeaderSlots === []

    -- As there are no BFT nodes, the next leadership schedule should match slots assigned exactly
  H.noteShow_ (expectedLeadershipSlotNumbers \\ leaderSlots)
  H.assert $ L.null (expectedLeadershipSlotNumbers \\ leaderSlots)
    -- TODO: Re-enable --next leadership schedule test
    {-

  id do
    nextLeaderShipScheduleFile <- H.noteTempFile work "next-schedule.log"

    leadershipScheduleDeadline <- H.noteShowM $ DTC.addUTCTime 180 <$> H.noteShowIO DTC.getCurrentTime
    -- TODO: Current works, next is failing

    H.byDeadlineM 5 leadershipScheduleDeadline "Failed to query for leadership schedule" $ do
      void $ execCli' execConfig
        [ "latest", "query", "leadership-schedule"
        , "--genesis", shelleyGenesisFile tr
        , "--stake-pool-id", stakePoolIdNewSpo
        , "--vrf-signing-key-file", vrfSkey
        , "--out-file", nextLeaderShipScheduleFile
        , "--next"
        ]

    scheduleJson <- H.leftFailM $ H.readJsonFile nextLeaderShipScheduleFile

    expectedLeadershipSlotNumbers <- H.noteShowM $ fmap (fmap slotNumber) $ H.leftFail $ J.parseEither (J.parseJSON @[LeadershipSlot]) scheduleJson
    maxSlotExpected <- H.noteShow $ maximum expectedLeadershipSlotNumbers

    H.assert $ not (L.null expectedLeadershipSlotNumbers)

    leadershipDeadline <- H.noteShowM $ DTC.addUTCTime 90 <$> H.noteShowIO DTC.getCurrentTime

    -- We need enough time to pass such that the expected leadership slots generated by the
    -- leadership-schedule command have actually occurred.
    (leaderSlots, notLeaderSlots) <- H.byDeadlineM 10 leadershipDeadline "Wait for chain to surpass all expected leadership slots" $ do
      (someLeaderSlots, someNotLeaderSlots) <- getRelevantSlots testPoolStdOutFp (minimum expectedLeadershipSlotNumbers)
      if L.null someLeaderSlots
        then H.failure
        else do
          maxActualSlot <- H.noteShow $ maximum someLeaderSlots
          H.assert $ maxActualSlot >= maxSlotExpected
      pure (someLeaderSlots, someNotLeaderSlots)

    H.noteShow_ expectedLeadershipSlotNumbers
    H.noteShow_ leaderSlots
    H.noteShow_ notLeaderSlots

    -- Double check that we've seen all slots
    H.noteShow_ ("Slots not seen as TraceNodeIsLeader nor TraceNodeNotLeader" :: Text)
    ([minimum expectedLeadershipSlotNumbers .. maxSlotExpected] \\ leaderSlots) \\ notLeaderSlots === []

    -- As there are no BFT nodes, the next leadership schedule should match slots assigned exactly
    H.noteShow_ (expectedLeadershipSlotNumbers \\ leaderSlots)
    H.assert $ L.null (expectedLeadershipSlotNumbers \\ leaderSlots)
-}
