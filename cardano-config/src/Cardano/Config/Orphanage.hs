{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Config.Orphanage where

import           Cardano.Prelude
import qualified Prelude

import           Data.Aeson (FromJSON, parseJSON, Value(..))
import           Network.Socket (PortNumber)
import           Data.Scientific (coefficient)
import qualified Data.Text as T

import           Cardano.BM.Data.Tracer (TracingVerbosity(..))
import qualified Cardano.Chain.Update as Update
import qualified Ouroboros.Consensus.BlockchainTime as Consensus


deriving instance Num Consensus.SlotLength

deriving instance Show TracingVerbosity


instance FromJSON PortNumber where
  parseJSON (Number portNum) = case readMaybe . show $ coefficient portNum of
                                 Just port -> pure port
                                 Nothing -> panic $ (show portNum)
                                                  <> " is not a valid port number."
  parseJSON invalid  = panic $ "Parsing of port number failed due to type mismatch. "
                             <> "Encountered: " <> (T.pack $ Prelude.show invalid)

instance FromJSON Update.ApplicationName where
  parseJSON (String x) = pure $ Update.ApplicationName x
  parseJSON invalid  =
    panic $ "Parsing of application name failed due to type mismatch. "
    <> "Encountered: " <> (T.pack $ Prelude.show invalid)
