{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Tedious.Entity where

import Control.Exception (Exception)
import Control.Lens (makeLenses, (&), (.~), (<&>), (?~), (^.))
import Data.Aeson (ToJSON (..))
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

data PageO a = PageO
  { _pageOPage :: Page,
    _pageOTotal :: Natural,
    _pageOData :: a
  }

deriving stock instance (Show a) => Show (PageO a)

deriving stock instance (Eq a) => Eq (PageO a)

deriving stock instance Generic (PageO a)

deriveJSON (toJSONOptions {A.fieldLabelModifier = trimPrefixName_ "PageO"}) ''PageO

instance (ToSchema a) => ToSchema (PageO a) where
  declareNamedSchema = genericDeclareNamedSchema (schemaOptions {O.fieldLabelModifier = trimPrefixName_ "PageO"})

makeLenses ''PageO

data PageI a = PageI
  { _pageIPage :: Maybe Page,
    _pageIFilter :: Maybe a
  }

deriving stock instance (Show a) => Show (PageI a)

deriving stock instance (Eq a) => Eq (PageI a)

deriving stock instance Generic (PageI a)

deriveJSON (toJSONOptions {A.fieldLabelModifier = trimPrefixName_ "PageI"}) ''PageI

instance (ToSchema a) => ToSchema (PageI a) where
  declareNamedSchema _ = do
    pageSchema <- declareSchemaRef (Proxy :: Proxy (Maybe Page))
    filtSchema <- declareSchemaRef (Proxy :: Proxy (Maybe a))
    return $
      named "PageI" $
        mempty
          & type_ ?~ OpenApiObject
          & properties
            .~ fromList
              [ ("page", pageSchema <&> title ?~ "分页"),
                ("filter", filtSchema <&> title ?~ "过滤条件")
              ]
          & required .~ ["index", "size"]
          & example ?~ toJSON (PageI (Just $ Page 1 10) (Just def) :: PageI ())

makeLenses ''PageI

fillPage :: Page -> Natural -> b -> PageO b
fillPage page total v =
  PageO
    { _pageOPage = page,
      _pageOTotal = total,
      _pageOData = v
    }

--

data Rep a = Rep
  { _repCode :: Natural,
    _repMessage :: Text,
    _repData :: Maybe a
  }

deriving stock instance (Show a) => Show (Rep a)

deriving stock instance (Eq a) => Eq (Rep a)

deriving stock instance Generic (Rep a)

deriveJSON (toJSONOptions {A.fieldLabelModifier = trimPrefixName_ "Rep"}) ''Rep

instance (ToSchema a) => ToSchema (Rep a) where
  declareNamedSchema = genericDeclareNamedSchema schemaOptions {O.fieldLabelModifier = trimPrefixName_ "Rep"}

makeLenses ''Rep

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
