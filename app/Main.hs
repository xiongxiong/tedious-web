{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}

module Main where

import Control.Exception (Exception)
import Data.Aeson.Text (encodeToLazyText)
import Data.OpenApi (toSchema)
import Data.OpenApi.Internal.Utils (encodePretty)
import Data.Profunctor.Product
import Data.Proxy (Proxy (..))
import Data.Tagged (Tagged)
import Data.Text (Text)
import Data.Text.Lazy (unpack)
import Data.Text.Lazy.Encoding (decodeUtf8)
import GHC.Generics (Generic)
import Opaleye (Field, FieldNullable, SqlText)
import Tedious (tt)
import Text.RawString.QQ (r)

[tt|
  Dog table "my_dog" -- dog
    firstName `first name` Text `Wang` ("first_name", Field SqlText) DogA DogC
    secondName `second name` Text `Ji Rong` ("second_name", Maybe (Field SqlText), Field SqlText) DogA
    address `home address` Text `Beijing` (Maybe (Field Text), Field Text) DogA -- where the dog is live in
    master `master's name` Text? (FieldNullable Text) DogA DogB? -- master
    masterAge `master's age` Int `26` DogC -- age of dog's master
    age `dog's age` Int `8` DogA DogB?
    hobby `dog's hobby` (Maybe Text)? `Just "meat"` (FieldNullable Text) DogC? DogB
    color `dog's color` Text DogB DogC

  Cat
    name Text CatA CatC
    master Text? CatA CatB?
    age Int CatA CatB?
    hobby (Maybe Text)? CatC? CatB
    color Text CatB CatC

  Page
    index `page index` Int `1`
    size `page size` Int `10`

  Pig deriving Exception
    name `pig's name` Text
|]

main :: IO ()
main = return ()

testToJson :: IO ()
testToJson = putStrLn . unpack . encodeToLazyText $ Just ("hello" :: String)

testToSchema :: IO ()
testToSchema = do
  putStrLn . unpack . decodeUtf8 . encodePretty $ toSchema (Proxy :: Proxy (Tagged "hello" Int))
  putStrLn . unpack . decodeUtf8 . encodePretty $ toSchema (Proxy :: Proxy Page)

str :: String
str =
  [r|"name"|]