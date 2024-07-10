{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE Arrows #-}

module Tedious.Handler where

import Control.Arrow (returnA)
import Control.Lens ((^.))
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Pool (Pool, withResource)
import Data.Profunctor.Product.Default (Default)
import Data.Text (Text, pack)
import Data.Time (getCurrentTime)
import Data.Tuple.All (Sel1 (..))
import Data.Typeable (Proxy (..), Typeable, typeRep)
import Database.PostgreSQL.Simple (Connection)
import Effectful (Eff, IOE, MonadIO (..), (:>))
import Effectful.Error.Dynamic (Error, throwError)
import Effectful.Reader.Dynamic (Reader, asks)
import Network.HTTP.Types qualified as HTTP
import Opaleye (DefaultFromField, Delete (Delete, dReturning, dTable, dWhere), Field, FromFields, Insert (Insert, iOnConflict, iReturning, iRows, iTable), Order, SqlBool, SqlInt8, Table, Unpackspec, Update (Update, uReturning, uTable, uUpdateWith, uWhere), countRows, limit, offset, orderBy, rCount, rReturning, runDelete, runInsert, runSelect, runUpdate, selectTable, sqlStrictText, sqlUTCTime, toNullable, where_, (.==))
import Tedious.Entity (Err (Err), Page (Page), PageI (_pageIFilter, _pageIPage), PageO (PageO), Rep, SysOper' (..), catchRep, fillPage, pageIndex, pageSize, rep, repErr, repOk, sysOperTable)
import WebGear.Core (BasicAuthError (..), Body, Description, Gets, Handler (arrM, setDescription, setSummary), HasTrait (from), HaveTraits, JSON (JSON), Middleware, PathVar, PlainText (..), Request, RequestHandler, RequiredResponseHeader, Response, Sets, StdHandler, Summary, With, pick, requestBody, respondA, (<<<))

withDoc :: (Handler h m) => Summary -> Description -> Middleware h ts ts
withDoc summ descr handler = setDescription descr <<< setSummary summ <<< handler

errorHandler ::
  ( Show e,
    StdHandler h m,
    Sets h '[RequiredResponseHeader "Content-Type" Text, Body JSON (Rep ())]
  ) =>
  h (Request `With` ts, e) Response
errorHandler = proc (_, err) -> respondA HTTP.ok200 JSON -< (repErr 1 (pack . show $ err) :: Rep ())

-- authFail ::
--   ( StdHandler h App,
--     Sets h '[RequiredResponseHeader "WWW-Authenticate" Text]
--   ) =>
--   h (Request `With` ts, BasicAuthError ()) Response
-- authFail = respondUnauthorized "Basic" "MyRealm"

authFail ::
  ( StdHandler h (Eff es)
  ) =>
  h (Request `With` ts, BasicAuthError ()) Response
authFail = proc (_request, err) -> case err of
  BasicAuthAttributeError () ->
    respondA HTTP.forbidden403 PlainText -< "Forbidden" :: Text
  _ ->
    respondA HTTP.unauthorized401 PlainText -< "Unauthorized" :: Text

audit ::
  forall eff env es h ts. -- eff effects (app env) arrow traits
  ( Reader env :> es,
    IOE :> es,
    eff ~ Eff es,
    StdHandler h eff
  ) =>
  (env -> Pool Connection) ->
  Maybe Text ->
  (RequestHandler h ts, SysOper') ->
  RequestHandler h ts
audit envPool user (handler, oper) =
  proc request -> do
    res <- handler -< request
    arrM
      ( const $ do
          now <- liftIO getCurrentTime
          pool <- asks envPool
          liftIO . withResource pool $ \conn -> do
            runInsert
              conn
              Insert
                { iTable = sysOperTable,
                  iRows =
                    [ ( Nothing,
                        toNullable . sqlStrictText <$> user,
                        sqlStrictText . _sysOper'Name $ oper,
                        sqlStrictText . _sysOper'Target $ oper,
                        toNullable . sqlStrictText <$> _sysOper'Content oper,
                        sqlUTCTime now
                      )
                    ],
                  iReturning = rReturning (const ()),
                  iOnConflict = Nothing
                }
      )
      -<
        ()
    returnA -< res

list ::
  forall i d f ids t fs r eff env es h ts. -- (record id) data (data filter) [id] table (table fields) (table record) eff effects (app env) arrow traits
  ( Typeable t,
    ids ~ [i],
    Integral i,
    DefaultFromField SqlInt8 i,
    Default Unpackspec fs fs,
    Default FromFields fs r,
    Reader env :> es,
    IOE :> es,
    eff ~ Eff es,
    StdHandler h eff,
    Gets h '[Body JSON (PageI f)],
    Sets h '[RequiredResponseHeader "Content-Type" Text, Body JSON (Rep (PageO [d])), Body JSON (Rep ())]
  ) =>
  (env -> Pool Connection) ->
  Table t fs ->
  (Maybe f -> fs -> Field SqlBool) ->
  Order fs ->
  (r -> d) ->
  (RequestHandler h ts, SysOper')
list envPool tbl flt ord r2d =
  let handler = requestBody @(PageI f) JSON errorHandler $
        proc request -> do
          let pageI = pick @(Body JSON (PageI f)) $ from request
          res <-
            arrM
              ( \pageI -> catchRep $ do
                  let mPage = _pageIPage pageI
                  pool <- asks envPool
                  (rs, cs) <- liftIO . withResource pool $
                    \conn -> do
                      let sel = do
                            r <- selectTable tbl
                            where_ $ flt (_pageIFilter pageI) r
                            pure r
                      let sel' = case mPage of
                            Nothing -> sel
                            Just page -> limit (fromIntegral $ page ^. pageSize) . offset (fromIntegral $ (page ^. pageIndex - 1) * (page ^. pageSize)) . orderBy ord $ sel
                      rs <- runSelect conn sel'
                      cs <- runSelect conn . countRows $ selectTable tbl
                      return (rs, fromIntegral <$> (cs :: ids))
                  let count = fromMaybe 0 (listToMaybe cs)
                  let pageO = case mPage of
                        Nothing -> PageO (Page 1 count) count
                        Just page -> fillPage page count
                  return . rep . pageO $ r2d <$> rs
              )
              -<
                pageI
          respondA HTTP.ok200 JSON -< res
      oper = SysOper' "list" (pack . show . typeRep $ (Proxy :: Proxy t)) Nothing
   in (handler, oper)

get ::
  forall i fi d t fs r eff env es h ts. -- (record id) (field id) data table (table fields) (table record) eff effects (app env) arrow traits
  ( Typeable t,
    Default Unpackspec fs fs,
    Default FromFields fs r,
    Sel1 fs (Field fi),
    Reader env :> es,
    IOE :> es,
    eff ~ Eff es,
    StdHandler h eff,
    HaveTraits '[PathVar "id" i] ts,
    Sets h '[RequiredResponseHeader "Content-Type" Text, Body JSON (Rep (Maybe d))]
  ) =>
  (env -> Pool Connection) ->
  Table t fs ->
  (i -> Field fi) ->
  (r -> d) ->
  (RequestHandler h ts, SysOper')
get envPool tbl idf r2d =
  let handler = proc request -> do
        let tid = pick @(PathVar "id" i) $ from request
        (md :: Maybe d) <-
          arrM
            ( \tid -> do
                pool <- asks envPool
                rs <- liftIO . withResource pool $
                  \conn -> runSelect conn $ do
                    r <- selectTable tbl
                    where_ $ sel1 r .== idf tid
                    pure r
                pure . listToMaybe $ r2d <$> rs
            )
            -<
              tid
        respondA HTTP.ok200 JSON -< rep md
      oper = SysOper' "get" (pack . show . typeRep $ (Proxy :: Proxy t)) Nothing
   in (handler, oper)

add ::
  forall i fi a t fs eff env es h ts. -- (record id) (field id) (data to add) table (table fields) eff effects (app env) arrow traits
  ( Typeable t,
    Sel1 fs (Field fi),
    DefaultFromField fi i,
    Reader env :> es,
    IOE :> es,
    eff ~ Eff es,
    StdHandler h eff,
    Gets h '[Body JSON a],
    Sets h '[RequiredResponseHeader "Content-Type" Text, Body JSON (Rep i), Body JSON (Rep ())]
  ) =>
  (env -> Pool Connection) ->
  Table t fs ->
  (a -> [t]) ->
  (RequestHandler h ts, SysOper')
add envPool tbl a2t =
  let handler = requestBody @a JSON errorHandler $
        proc request -> do
          let toAdd = pick @(Body JSON a) $ from request
          tid <-
            arrM
              ( \toAdd -> catchRep $ do
                  pool <- asks envPool
                  ids <- liftIO . withResource pool $ \conn -> do
                    runInsert
                      conn
                      Insert
                        { iTable = tbl,
                          iRows = a2t toAdd,
                          iReturning = rReturning sel1,
                          iOnConflict = Nothing
                        }
                  maybe (throwError $ Err 1 "insert failure") (return . rep) (listToMaybe ids)
              )
              -<
                toAdd
          respondA HTTP.ok200 JSON -< (tid :: Rep i)
      oper = SysOper' "add" (pack . show . typeRep $ (Proxy :: Proxy t)) Nothing
   in (handler, oper)

dup ::
  forall i fi t fs r eff env es h ts. -- (record id) (field id) table (table fields) (table record) eff effects (app env) arrow traits
  ( Typeable t,
    Default Unpackspec fs fs,
    Default FromFields fs r,
    Sel1 fs (Field fi),
    DefaultFromField fi i,
    Reader env :> es,
    IOE :> es,
    eff ~ Eff es,
    StdHandler h eff,
    HaveTraits '[PathVar "id" i] ts,
    Sets h '[RequiredResponseHeader "Content-Type" Text, Body JSON (Rep i), Body JSON (Rep ())]
  ) =>
  (env -> Pool Connection) ->
  Table t fs ->
  (i -> Field fi) ->
  (r -> t) ->
  (RequestHandler h ts, SysOper')
dup envPool tbl idf r2t =
  let handler = proc request -> do
        let tid = pick @(PathVar "id" i) $ from request
        tid_ <-
          arrM
            ( \tid -> catchRep $ do
                pool <- asks envPool
                ids <- liftIO . withResource pool $
                  \conn -> do
                    rs <- runSelect conn $ do
                      r <- selectTable tbl
                      where_ $ sel1 r .== idf tid
                      pure r
                    runInsert
                      conn
                      Insert
                        { iTable = tbl,
                          iRows = r2t <$> rs,
                          iReturning = rReturning sel1,
                          iOnConflict = Nothing
                        }
                maybe (throwError $ Err 1 "duplicate failure") (return . rep) (listToMaybe ids)
            )
            -<
              tid
        respondA HTTP.ok200 JSON -< (tid_ :: Rep i)
      oper = SysOper' "dup" (pack . show . typeRep $ (Proxy :: Proxy t)) Nothing
   in (handler, oper)

upd ::
  forall i fi u t fs d r eff env es h ts. -- (record id) (field id) update table (table fields) data (table record) eff effects (app env) arrow traits
  ( Typeable t,
    Sel1 fs (Field fi),
    Default FromFields fs r,
    Reader env :> es,
    IOE :> es,
    eff ~ Eff es,
    StdHandler h eff,
    HaveTraits '[PathVar "id" i] ts,
    Gets h '[Body JSON u],
    Sets h '[RequiredResponseHeader "Content-Type" Text, Body JSON (Rep d), Body JSON (Rep ())]
  ) =>
  (env -> Pool Connection) ->
  Table t fs ->
  (i -> Field fi) ->
  (u -> (fs -> t)) ->
  (r -> d) ->
  (RequestHandler h ts, SysOper')
upd envPool tbl idf u2t r2d =
  let handler = requestBody @u JSON errorHandler $
        proc request -> do
          let tid = pick @(PathVar "id" i) $ from request
          let toUpd = pick @(Body JSON u) $ from request
          ru <-
            arrM
              ( \(tid, toUpd) -> catchRep $ do
                  pool <- asks envPool
                  ru <- liftIO . withResource pool $ \conn -> do
                    runUpdate
                      conn
                      Update
                        { uTable = tbl,
                          uUpdateWith = u2t toUpd,
                          uWhere = (.== idf tid) . sel1,
                          uReturning = rReturning id
                        }
                  maybe (throwError $ Err 1 "insert failure") (return . rep . r2d) (listToMaybe ru)
              )
              -<
                (tid, toUpd)
          respondA HTTP.ok200 JSON -< (ru :: Rep d)
      oper = SysOper' "upd" (pack . show . typeRep $ (Proxy :: Proxy t)) Nothing
   in (handler, oper)

del ::
  forall i fi t fs eff env es h ts. -- (record id) (field id) table (table fields) data (table fields) eff effects (app env) arrow traits
  ( Typeable t,
    Sel1 fs (Field fi),
    Reader env :> es,
    IOE :> es,
    eff ~ Eff es,
    StdHandler h eff,
    HaveTraits '[PathVar "id" i] ts,
    Sets h '[RequiredResponseHeader "Content-Type" Text, Body JSON (Rep Text), Body JSON (Rep ())]
  ) =>
  (env -> Pool Connection) ->
  Table t fs ->
  (i -> Field fi) ->
  (RequestHandler h ts, SysOper')
del envPool tbl idf =
  let handler = proc request -> do
        let tid = pick @(PathVar "id" i) $ from request
        r <-
          arrM
            ( \tid -> do
                pool <- asks envPool
                c <- liftIO . withResource pool $
                  \conn ->
                    runDelete conn $
                      Delete
                        { dTable = tbl,
                          dWhere = (.== idf tid) . sel1,
                          dReturning = rCount
                        }
                pure $ if c == 1 then repOk else repErr 1 "delete failure"
            )
            -<
              tid
        respondA HTTP.ok200 JSON -< (r :: Rep Text)
      oper = SysOper' "get" (pack . show . typeRep $ (Proxy :: Proxy t)) Nothing
   in (handler, oper)
