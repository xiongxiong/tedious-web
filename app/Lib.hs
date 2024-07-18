{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

module Lib where

import Control.Exception (Exception)
import Data.Int (Int64)
import Data.Profunctor.Product
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Opaleye (Field, FieldNullable, SqlText, SqlTimestamptz)
import Opaleye.SqlTypes (SqlInt8, SqlInt4)
import Tedious (tedious)

[tedious|
  Dog table dog deriving Exception -- dog
    id `ID` Int64 (Maybe (Field SqlInt8), Field SqlInt8)
    firstName `first name` Text `Wang` ("first_name", Field SqlText) !UniqueDog DogA DogC Dog_:a
    secondName `second name` Text `Ji Rong` ("second_name", Maybe (Field SqlText), Field SqlText) !UniqueDog DogA Dog_:b?
    address `home address` Text `Beijing` (Maybe (Field Text), Field Text) DogA Dog_:c -- where the dog is live in
    master `master's name` Text (Field Text) !UniqueDogMaster default=`'the lord'` DogA DogB? -- master
    masterAge `master's age` Int `26` DogC -- age of dog's master
    age `dog's age` Int `8` DogA DogB?
    hobby `dog's hobby` (Maybe Text)? `Just "meat"` (FieldNullable Text) DogC? DogB
    color `dog's color` ((Text, Text, Text)) DogB DogC
    friends `dog's friends` [Text] DogC
    createdAt `creation time` UTCTime ("cteated_at", Maybe (FieldNullable SqlTimestamptz), FieldNullable SqlTimestamptz) default=`CURRENT_TIMESTAMP` DogB DogC?

  Cat table (public, cat)
    id `ID` Int64 (Maybe (Field SqlInt8), Field SqlInt8)
    name Text (Maybe (Field Text), Field Text) !UniqueCat CatA CatC
    master Text? (Maybe (FieldNullable Text), FieldNullable Text) CatA CatB?
    age Int (Field SqlInt4) default=`0` CatA CatB?
    hobby (Maybe Text)? (Maybe (FieldNullable Text), FieldNullable Text) CatC? CatB
    color Text (Maybe (Field Text), Field Text) !UniqueCat CatB CatC
    mind a?

  Page
    index `page index` Int `1`
    size `page size` Int `10`

|]