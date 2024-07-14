{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import Data.Aeson.Text (encodeToLazyText)
import Data.OpenApi (toSchema)
import Data.OpenApi.Internal.Utils (encodePretty)
import Data.Proxy (Proxy (..))
import Data.Tagged (Tagged)
import Data.Text (Text)
import Data.Text.Lazy (unpack)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Time (UTCTime)
import Lib hiding (Dog, Cat)
import Database.Persist.TH (share, mkPersist, sqlSettings, mkMigrate, persistUpperCase)
import Language.Haskell.TH.Quote (quoteExp)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] $(quoteExp persistUpperCase tediousPersistString)

main :: IO ()
main = return ()

testToJson :: IO ()
testToJson = putStrLn . unpack . encodeToLazyText $ Just ("hello" :: String)

testToSchema :: IO ()
testToSchema = do
  putStrLn . unpack . decodeUtf8 . encodePretty $ toSchema (Proxy :: Proxy (Tagged "hello" Int))
  putStrLn . unpack . decodeUtf8 . encodePretty $ toSchema (Proxy :: Proxy Page)