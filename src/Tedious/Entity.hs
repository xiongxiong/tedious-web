{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Tedious.Entity where

import Control.Exception (Exception)
import Control.Lens ((^.))
import Control.Monad.Catch (MonadCatch (..))
import Data.Foldable (find)
import Data.Int (Int64)
import Data.Profunctor.Product
import Data.Text (Text, pack)
import Data.Text.Encoding (decodeUtf8)
import Data.Time (UTCTime)
import Database.PostgreSQL.Simple (SqlError (..))
import Effectful (Eff, (:>))
import Effectful.Error.Dynamic (Error, runErrorWith, throwError)
import Numeric.Natural (Natural)
import Opaleye (Field, FieldNullable, SqlInt8, SqlText, SqlTimestamptz)
import Tedious.Quasi (tedious)

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

SysUser
  name Text (Field SqlText)
  pass Text (Field SqlText)

SysOper
  id `ID` Int64 (Maybe (Field SqlInt8), Field SqlInt8)
  user `人员` Text? (Maybe (FieldNullable SqlText), FieldNullable SqlText)
  name `名称` Text (Field SqlText) SysOper'
  target `目标` Text (Field SqlText) SysOper'
  content `内容` Text? (Maybe (FieldNullable SqlText), FieldNullable SqlText) SysOper'
  time `时间` UTCTime (Maybe (Field SqlTimestamptz), Field SqlTimestamptz) default=`CURRENT_TIMESTAMP`
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

catchSqlError :: (Error Err :> es) => [(Text, Text)] -> Eff es (Rep a) -> Eff es (Rep a)
catchSqlError arms act = do
  catch act procErr
  where
    procErr (SqlError code _ msg detail _) = do
      let defErrMsg = "SqlError -- code : " <> show code <> ", msg : " <> show msg <> ", detail : " <> show detail
      let errMsg = maybe (pack defErrMsg) snd $ find ((== decodeUtf8 code) . fst) arms
      throwError $ Err 1 errMsg

catchSqlErrorRep :: [(Text, Text)] -> Eff (Error Err : es) (Rep a) -> Eff es (Rep a)
catchSqlErrorRep arms act = catchRep $ catchSqlError arms act

--

type SysOperTargetName = Text