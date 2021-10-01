-- | Internal utils for the other Api modules
--
module Cardano.Api.Utils
  ( (?!)
  , (?!.)
  , formatParsecError
  ) where

import           Prelude

import qualified Text.ParserCombinators.Parsec.Error as Parsec


(?!) :: Maybe a -> e -> Either e a
Nothing ?! e = Left e
Just x  ?! _ = Right x

(?!.) :: Either e a -> (e -> e') -> Either e' a
Left  e ?!. f = Left (f e)
Right x ?!. _ = Right x

formatParsecError :: Parsec.ParseError -> String
formatParsecError err =
  Parsec.showErrorMessages "or" "unknown parse error"
    "expecting" "unexpected" "end of input"
    $ Parsec.errorMessages err
