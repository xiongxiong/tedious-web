module Tedious.Util
  ( upperFirst,
    lowerFirst,
    trimPrefixName,
    trimPrefixName_,
    toJSONOptions,
    schemaOptions
  )
where

import Control.Lens (Ixed (..), (%~))
import Data.Char (toLower, toUpper)
import Data.Aeson (Options, defaultOptions)
import qualified Data.Aeson as A
import Data.OpenApi (fromAesonOptions, SchemaOptions)

upperFirst :: String -> String
upperFirst = ix 0 %~ toUpper

lowerFirst :: String -> String
lowerFirst = ix 0 %~ toLower

trimPrefixName :: String -> String -> String
trimPrefixName name = lowerFirst . drop (length name)

trimPrefixName_ :: String -> String -> String
trimPrefixName_ name = lowerFirst . drop 1 . drop (length name)

toJSONOptions :: Options
toJSONOptions = defaultOptions {A.allNullaryToStringTag = False, A.sumEncoding = A.UntaggedValue, A.constructorTagModifier = map toLower, A.fieldLabelModifier = drop 1, A.omitNothingFields = True}

schemaOptions :: SchemaOptions
schemaOptions = fromAesonOptions toJSONOptions