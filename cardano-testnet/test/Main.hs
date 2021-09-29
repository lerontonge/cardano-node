{-# LANGUAGE FlexibleInstances #-}

module Main
  ( main
  ) where

import           Prelude

import qualified System.Environment as E
import qualified Test.Tasty as T
import qualified Test.Tasty.Hedgehog as H
import qualified Test.Tasty.Ingredients as T

--import qualified Spec.Plutus.Direct.ScriptContextEquality
import qualified Spec.Plutus.Direct.ScriptContextEqualityMint
--import qualified Spec.Plutus.Direct.TxInLockingPlutus
--import qualified Spec.Plutus.Script.TxInLockingPlutus
--import qualified Spec.Plutus.SubmitApi.TxInLockingPlutus

tests :: IO T.TestTree
tests = do
  pure $ T.testGroup "test/Spec.hs"
    [ T.testGroup "Spec"
      [ -- H.testProperty "Spec.Plutus.Direct.TxInLockingPlutus" Spec.Plutus.Direct.TxInLockingPlutus.hprop_plutus
        -- , H.testProperty "Spec.Plutus.Script.TxInLockingPlutus" Spec.Plutus.Script.TxInLockingPlutus.hprop_plutus
        -- , H.testProperty "Spec.Plutus.SubmitApi.TxInLockingPlutus" Spec.Plutus.SubmitApi.TxInLockingPlutus.hprop_plutus
        -- , H.testProperty "Spec.Plutus.Direct.ScriptContextEquality"  Spec.Plutus.Direct.ScriptContextEquality.hprop_plutus_script_context_equality
       H.testProperty "Spec.Plutus.Direct.ScriptContextEqualityMint"  Spec.Plutus.Direct.ScriptContextEqualityMint.hprop_plutus_script_context_mint_equality

      ]
    ]

ingredients :: [T.Ingredient]
ingredients = T.defaultIngredients

main :: IO ()
main = do
  args <- E.getArgs

  E.withArgs args $ tests >>= T.defaultMainWithIngredients ingredients
