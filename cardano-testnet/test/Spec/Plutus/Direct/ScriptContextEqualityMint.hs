{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Spec.Plutus.Direct.ScriptContextEqualityMint
  ( hprop_plutus_script_context_mint_equality
  ) where

import           Control.Applicative
import           Control.Monad
import           Data.Aeson (FromJSON (..), Value, (.:))
import           Data.Eq
import           Data.Function
import           Data.Functor ((<&>))
import           Data.HashMap.Lazy (HashMap)
import           Data.Int
import           Data.Maybe
import           Data.Monoid (Last (..), (<>))
import           Data.Text (Text)
import           GHC.Num
import           GHC.Real
import           Hedgehog (Property, (/==))
import           Prelude (filter, head, (!!))
import           System.Environment (getEnvironment)
import           System.FilePath ((</>))
import           Text.Show (Show (..))

import qualified Data.Aeson as J
import qualified Data.HashMap.Lazy as HM
import qualified Data.Text as T
import qualified Hedgehog as H
import qualified Hedgehog.Extras.Stock.IO.Network.Sprocket as IO
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.Concurrent as H
import qualified Hedgehog.Extras.Test.File as H
import qualified Hedgehog.Extras.Test.Process as H
import qualified System.Directory as IO
import qualified Test.Base as H
import           Test.Process (execCreateScriptContext, execCreateScriptContext')
import qualified Test.Process as H
import           Testnet.Cardano (defaultTestnetOptions, testnet)
import qualified Testnet.Cardano as TC
import qualified Testnet.Conf as H

{- HLINT ignore "Redundant <&>" -}
{- HLINT ignore "Redundant return" -}
{- HLINT ignore "Use let" -}

data Utxo = Utxo
  { address :: Text
  , value :: HashMap Text Integer
  } deriving (Eq, Show)

instance FromJSON Utxo where
  parseJSON = J.withObject "Utxo" $ \v -> Utxo
    <$> v .: "address"
    <*> v .: "value"

hprop_plutus_script_context_mint_equality :: Property
hprop_plutus_script_context_mint_equality = H.integration . H.runFinallies . H.workspace "chairman" $ \tempAbsBasePath' -> do
  projectBase <- H.note =<< H.noteIO . IO.canonicalizePath =<< H.getProjectBase
  conf@H.Conf { H.tempBaseAbsPath, H.tempAbsPath } <- H.noteShowM $ H.mkConf tempAbsBasePath' Nothing

  TC.TestnetRuntime { bftSprockets, testnetMagic } <- testnet defaultTestnetOptions conf

  env <- H.evalIO getEnvironment

  execConfig <- H.noteShow H.ExecConfig
        { H.execConfigEnv = Last $ Just $
          [ ("CARDANO_NODE_SOCKET_PATH", IO.sprocketArgumentName (head bftSprockets))
          ]
          -- The environment must be passed onto child process on Windows in order to
          -- successfully start that process.
          <> env
        , H.execConfigCwd = Last $ Just tempBaseAbsPath
        }

  -- First we note all the relevant files
  base <- H.note projectBase
  work <- H.note tempAbsPath

  -- We get our UTxOs from here
  utxoVKeyFile <- H.note $ tempAbsPath </> "shelley/utxo-keys/utxo1.vkey"
  utxoSKeyFile <- H.note $ tempAbsPath </> "shelley/utxo-keys/utxo1.skey"
  scriptDummyRedeemer <- H.note $ work </> "mint-script-context-dummy.redeemer"
  scriptContextRedeemer <- H.note $ work </> "mint-script-context.redeemer"
  requiredSignerSKey <- H.note $ tempAbsPath </> "addresses/user1.skey"
  requiredSignerVKey <- H.note $ tempAbsPath </> "addresses/user1.vkey"
  plutusContextEquivalenceMintScript <- H.note $ base </> "scripts/plutus/scripts/minting-context-equivalance-test.plutus"

  policyId <- H.execCli
                [ "transaction", "policyid"
                , "--script-file", plutusContextEquivalenceMintScript
                ]

  void . H.note $ "Policy ID: " <> policyId

  H.noteEachM_ . H.listDirectory $ base
  H.noteEachM_ . H.listDirectory $ base </> "scripts"
  H.noteEachM_ . H.listDirectory $ base </> "scripts/plutus"
  H.noteEachM_ . H.listDirectory $ base </> "scripts/plutus/scripts"

  -- Step 1: Create a tx ouput with a datum hash at the script address. In order for a tx ouput to be locked
  -- by a plutus script, it must have a datahash. We also need collateral tx inputs so we split the utxo
  -- in order to accomodate this.


  utxoAddr <- H.execCli
    [ "address", "build"
    , "--testnet-magic", show @Int testnetMagic
    , "--payment-verification-key-file", utxoVKeyFile
    ]

  void $ H.execCli' execConfig
    [ "query", "utxo"
    , "--address", utxoAddr
    , "--cardano-mode"
    , "--testnet-magic", show @Int testnetMagic
    , "--out-file", work </> "utxo-1.json"
    ]

  H.cat $ work </> "utxo-1.json"

  utxo1Json <- H.leftFailM . H.readJsonFile $ work </> "utxo-1.json"
  utxo1 <- H.noteShowM $ H.jsonErrorFail $ J.fromJSON @(HashMap Text Utxo) utxo1Json
  txin <- H.noteShow $ head $ HM.keys utxo1
  lovelaceAtTxin <- H.nothingFailM . H.noteShow $ utxo1 & HM.lookup txin <&> value >>= HM.lookup "lovelace"
  lovelaceAtTxinDiv3 <- H.noteShow $ lovelaceAtTxin `div` 3

  void $ H.execCli' execConfig
    [ "query", "protocol-parameters"
    , "--testnet-magic", show @Int testnetMagic
    , "--out-file", work </> "pparams.json"
    ]

  let dummyaddress = "addr_test1vpqgspvmh6m2m5pwangvdg499srfzre2dd96qq57nlnw6yctpasy4"


  -- STEP 1 - Create collateral
  void $ H.execCli' execConfig
    [ "transaction", "build"
    , "--alonzo-era"
    , "--cardano-mode"
    , "--testnet-magic", show @Int testnetMagic
    , "--change-address", utxoAddr
    , "--tx-in", T.unpack txin
    , "--tx-out", utxoAddr <> "+" <> show @Integer lovelaceAtTxinDiv3
    , "--protocol-params-file", work </> "pparams.json"
    , "--out-file", work </> "create-collateral-output.body"
    ]

  void $ H.execCli
    [ "transaction", "sign"
    , "--tx-body-file", work </> "create-collateral-output.body"
    , "--testnet-magic", show @Int testnetMagic
    , "--signing-key-file", utxoSKeyFile
    , "--out-file", work </> "create-collateral-output.tx"
    ]

  void $ H.execCli' execConfig
    [ "transaction", "submit"
    , "--tx-file", work </> "create-collateral-output.tx"
    , "--testnet-magic", show @Int testnetMagic
    ]

  H.threadDelay 5000000

  -- STEP 2
  -- We need to create a dummy tx in order to create the script context redeemer
  -- that we want to use when attempting to spend the spending script locked UTxO

  void $ H.execCli' execConfig
    [ "query", "utxo"
    , "--address", utxoAddr
    , "--cardano-mode"
    , "--testnet-magic", show @Int testnetMagic
    , "--out-file", work </> "utxo-2.json"
    ]

  H.cat $ work </> "utxo-2.json"

  utxo2Json :: Value <- H.leftFailM $ H.readJsonFile $ work </> "utxo-2.json"
  utxo2 <- H.noteShowM $ H.jsonErrorFail $ J.fromJSON @(HashMap Text Utxo) utxo2Json
  txinFunding <- H.noteShow $ HM.keys utxo2 !! 0
  txinCollateral <- H.noteShow $ HM.keys utxo2 !! 1

  void $ execCreateScriptContext ["--out-file", scriptDummyRedeemer]

  H.cat $ work </> scriptDummyRedeemer

  void $ H.execCli' execConfig
    [ "transaction", "build"
    , "--alonzo-era"
    , "--cardano-mode"
    , "--script-invalid"
    , "--testnet-magic", show @Int testnetMagic
    , "--change-address", utxoAddr
    , "--invalid-before", "1"
    , "--invalid-hereafter", "3000"
    , "--required-signer", requiredSignerSKey
    , "--tx-in", T.unpack txinFunding
    , "--tx-in-collateral", T.unpack txinCollateral
    , "--mint-script-file", plutusContextEquivalenceMintScript
    , "--mint-redeemer-file", scriptDummyRedeemer
    , "--tx-out", dummyaddress <> "+" <> show @Integer 10000000 <> "+ 5 " <> filter (/= '\n') (policyId <> ".MillarCoin")
    , "--mint", "5 " <> filter (/= '\n') (policyId <> ".MillarCoin")
    , "--protocol-params-file", work </> "pparams.json"
    , "--out-file", work </> "mint-dummy.body"
    ]

  void $ H.execCli
    [ "transaction", "sign"
    , "--tx-body-file", work </> "mint-dummy.body"
    , "--testnet-magic", show @Int testnetMagic
    , "--signing-key-file", utxoSKeyFile
    , "--out-file", work </> "mint-dummy.tx"
    ]

  -- Generate the redeeemer we will use in the tx!
  void $ execCreateScriptContext' execConfig
           [ "--generate-tx" , work </> "mint-dummy.tx"
           , "--cardano-mode"
           , "--testnet-magic", show @Int testnetMagic
           , "--out-file", scriptContextRedeemer
           ]

  H.cat $ work </> scriptContextRedeemer

  H.threadDelay 5000000

  void $ H.execCli' execConfig
    [ "transaction", "build"
    , "--alonzo-era"
    , "--cardano-mode"
    , "--script-valid"
    , "--testnet-magic", show @Int testnetMagic
    , "--change-address", utxoAddr
    , "--invalid-before", "1"
    , "--invalid-hereafter", "3000"
    , "--required-signer", requiredSignerSKey
    , "--tx-in", T.unpack txinFunding
    , "--tx-in-collateral", T.unpack txinCollateral
    , "--mint-script-file", plutusContextEquivalenceMintScript
    , "--mint-redeemer-file", scriptContextRedeemer
    , "--tx-out", dummyaddress <> "+" <> show @Integer 10000000 <> "+ 5 " <> filter (/= '\n') (policyId <> ".MillarCoin")
    , "--mint", "5 " <> filter (/= '\n') (policyId <> ".MillarCoin")
    , "--protocol-params-file", work </> "pparams.json"
    , "--out-file", work </> "mint-final.body"
    ]

  void $ H.execCli
    [ "transaction", "sign"
    , "--tx-body-file", work </> "mint-final.body"
    , "--testnet-magic", show @Int testnetMagic
    , "--signing-key-file", utxoSKeyFile
    , "--signing-key-file", requiredSignerSKey
    , "--out-file", work </> "mint-final.tx"
    ]


  void $ H.execCli' execConfig
    [ "transaction", "submit"
    , "--tx-file", work </> "mint-final.tx"
    , "--testnet-magic", show @Int testnetMagic
    ]

  H.threadDelay 5000000

  -- Query UTxO at dummyAddress. If there is ADA there then the script context script was successful

  void $ H.execCli' execConfig
    [ "query", "utxo"
    , "--address", dummyaddress
    , "--testnet-magic", show @Int testnetMagic
    , "--out-file", work </> "dummyaddress.json"
    ]

  H.cat $ work </> "dummyaddress.json"

  dummyUtxoJson <- H.leftFailM . H.readJsonFile $ work </> "dummyaddress.json"
  -- TODO: Left off here. You need an updated Utxo type to incorporate multiassets (The JSON parser fails)
  dummyUtxo <- H.noteShowM $ H.jsonErrorFail $ J.fromJSON @(HashMap Text Utxo) dummyUtxoJson

  -- I.E the dummy address should have some ADA if the script context test was successful.
  dummyUtxo /== HM.empty
