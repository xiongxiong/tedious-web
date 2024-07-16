{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Tedious.Entity where

import Control.Exception (Exception)
import Control.Lens (makeLenses, (&), (.~), (<&>), (?~), (^.))
import Data.Aeson (ToJSON (..), FromJSON (..))
import Data.Aeson qualified as A
import Data.Aeson.TH (deriveJSON)
import Data.Default (Default (..))
import Data.HashMap.Strict.InsOrd (fromList)
import Data.Int (Int64)
import Data.OpenApi (HasExample (..), HasProperties (..), HasRequired (..), HasTitle (..), HasType (..), OpenApiType (..), ToSchema, declareSchemaRef, genericDeclareNamedSchema)
import Data.OpenApi qualified as O
import Data.OpenApi.Internal.Schema (named)
import Data.Profunctor.Product
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Time (UTCTime)
import Effectful (Eff)
import Effectful.Error.Dynamic (Error, runErrorWith)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import Opaleye (Field, FieldNullable, SqlInt8, SqlText, SqlTimestamptz)
import Tedious.Parser (tedious)
import Tedious.Util (schemaOptions, toJSONOptions, trimPrefixName_)

[tedious|
Page
  index `页码` Natural `1`
  size `每页条数` Natural `10`

PageO
  page `分页` Page
  total `总数` Natural
  data `数据` a

PageI
  page `分页` Page?
  filter `过滤` a?

Rep
  code `错误码` Natural
  message `消息` Text
  data `数据` a?

Err deriving Exception
  code Natural
  message Text

SysAdmin
  name Text
  pass Text

SysUser
  name Text (Field SqlText)
  pass Text (Field SqlText)

SysOper
  id `ID` Int64 (Maybe (Field SqlInt8), Field SqlInt8)
  user `人员` Text? (Maybe (FieldNullable SqlText), FieldNullable SqlText)
  name `名称` Text (Field SqlText) SysOper'
  target `目标` Text (Field SqlText) SysOper'
  content `内容` Text? (Maybe (FieldNullable SqlText), FieldNullable SqlText) SysOper'
  time `时间` UTCTime (Field SqlTimestamptz)
|]

fillPage :: Page -> Natural -> b -> PageO b
fillPage page total v =
  PageO
    { _pageOPage = page,
      _pageOTotal = total,
      _pageOData = v
    }

--

repOk :: Rep a
repOk = Rep 0 "ok" Nothing

rep :: a -> Rep a
rep d = Rep 0 "ok" (Just d)

repErr :: Natural -> Text -> Rep a
repErr c m = Rep c m Nothing

repErr' :: Err -> Rep a
repErr' e = Rep (e ^. errCode) (e ^. errMessage) Nothing

repErrNotSupport :: Rep a
repErrNotSupport = repErr 500 "not supported yet"

catchRep :: Eff (Error Err : es) (Rep a) -> Eff es (Rep a)
catchRep = runErrorWith (const $ return . repErr')

--

type SysOperTargetName = Text