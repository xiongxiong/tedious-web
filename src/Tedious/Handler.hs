{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE Arrows #-}

module Tedious.Handler
  ( withDoc,
    withDoc',
    errorHandler,
    basicAuthFail,
    audit,
    list,
    get,
    add,
    dup,
    upd,
    del,
  )
where

import Control.Arrow (returnA)
import Control.Lens ((^.))
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Pool (Pool, withResource)
import Data.Profunctor.Product.Default (Default)
import Data.Text (Text, pack)
import Data.Tuple.All (Sel1 (..))
import Database.PostgreSQL.Simple (Connection)
import Effectful (Eff, IOE, MonadIO (..), (:>))
import Effectful.Error.Dynamic (throwError)
import Effectful.Reader.Dynamic (Reader, asks)
import Network.HTTP.Types qualified as HTTP
import Opaleye (DefaultFromField, Delete (Delete, dReturning, dTable, dWhere), Field, FromFields, Insert (Insert, iOnConflict, iReturning, iRows, iTable), Order, SqlBool, SqlInt8, Table, Unpackspec, Update (Update, uReturning, uTable, uUpdateWith, uWhere), countRows, limit, offset, orderBy, rCount, rReturning, runDelete, runInsert, runSelect, runUpdate, selectTable, sqlStrictText, toNullable, where_, (.==))
import Opaleye.Internal.Table (tableIdentifier)
import Tedious.Entity (Err (Err), Page (Page), PageI (_pageIFilter, _pageIPage), PageO (PageO), Rep, SysOper' (..), catchRep, fillPage, pageIndex, pageSize, rep, repErr, repOk, sysOperTable, catchSqlErrorRep)
import WebGear.Core (BasicAuthError (..), Body, Description, Gets, Handler (arrM, setDescription, setSummary), HasTrait (from), HaveTraits, JSON (JSON), Middleware, PathVar, PlainText (..), Request, RequestHandler, RequiredResponseHeader, Response, Sets, StdHandler, Summary, With, pick, requestBody, respondA, (<<<))

withDoc :: (Handler h m) => Summary -> Description -> Middleware h ts ts
withDoc summ desc handler = setDescription desc <<< setSummary summ <<< handler

withDoc' :: (Handler h m) => Summary -> Middleware h ts ts
withDoc' summ handler = setSummary summ <<< handler

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

basicAuthFail ::
  ( StdHandler h (Eff es)
  ) =>
  h (Request `With` ts, BasicAuthError ()) Response
basicAuthFail = proc (_, err) -> case err of
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
                        Nothing
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
  forall i d f ids wfs rfs r eff env es h ts. -- (record id) data (data filter) [id] (table write fields) (table read fields) (table record) eff effects (app env) arrow traits
  ( ids ~ [i],
    Integral i,
    DefaultFromField SqlInt8 i,
    Default Unpackspec rfs rfs,
    Default FromFields rfs r,
    Reader env :> es,
    IOE :> es,
    eff ~ Eff es,
    StdHandler h eff,
    Gets h '[Body JSON (PageI f)],
    Sets h '[RequiredResponseHeader "Content-Type" Text, Body JSON (Rep (PageO [d])), Body JSON (Rep ())]
  ) =>
  (env -> Pool Connection) ->
  Table wfs rfs ->
  (Maybe f -> rfs -> Field SqlBool) ->
  Order rfs ->
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
      sysOper = SysOper' "list" (pack . show . tableIdentifier $ tbl) Nothing
   in (handler, sysOper)

get ::
  forall i fi d wfs rfs r eff env es h ts. -- (record id) (field id) data (table write fields) (table read fields) (table record) eff effects (app env) arrow traits
  ( Default Unpackspec rfs rfs,
    Default FromFields rfs r,
    Sel1 rfs (Field fi),
    Reader env :> es,
    IOE :> es,
    eff ~ Eff es,
    StdHandler h eff,
    HaveTraits '[PathVar "id" i] ts,
    Sets h '[RequiredResponseHeader "Content-Type" Text, Body JSON (Rep (Maybe d))]
  ) =>
  (env -> Pool Connection) ->
  Table wfs rfs ->
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
      sysOper = SysOper' "get" (pack . show . tableIdentifier $ tbl) Nothing
   in (handler, sysOper)

add ::
  forall i fi a wfs rfs eff env es h ts. -- (record id) (field id) (data to add) (table write fields) (table read fields) eff effects (app env) arrow traits
  ( Sel1 rfs (Field fi),
    DefaultFromField fi i,
    Reader env :> es,
    IOE :> es,
    eff ~ Eff es,
    StdHandler h eff,
    Gets h '[Body JSON a],
    Sets h '[RequiredResponseHeader "Content-Type" Text, Body JSON (Rep i), Body JSON (Rep ())]
  ) =>
  (env -> Pool Connection) ->
  (a -> eff (Either Err a)) ->
  Table wfs rfs ->
  (a -> [wfs]) ->
  [(Text, Text)] ->
  (RequestHandler h ts, SysOper')
add envPool chk tbl a2t sqlErrArms =
  let handler = requestBody @a JSON errorHandler $
        proc request -> do
          let toAdd = pick @(Body JSON a) $ from request
          rep_ <-
            arrM
              ( \toAdd -> do
                  chkRes <- chk toAdd
                  catchSqlErrorRep sqlErrArms $ do
                    toAdd_ <- either throwError pure chkRes
                    pool <- asks envPool
                    ids <- liftIO . withResource pool $ \conn -> do
                      runInsert
                        conn
                        Insert
                          { iTable = tbl,
                            iRows = a2t toAdd_,
                            iReturning = rReturning sel1,
                            iOnConflict = Nothing
                          }
                    maybe (throwError $ Err 1 "insert failure") (return . rep) (listToMaybe ids)
              )
              -<
                toAdd
          respondA HTTP.ok200 JSON -< (rep_ :: Rep i)
      sysOper = SysOper' "add" (pack . show . tableIdentifier $ tbl) Nothing
   in (handler, sysOper)

dup ::
  forall i fi wfs rfs r eff env es h ts. -- (record id) (field id) (table write fields) (table read fields) (table record) eff effects (app env) arrow traits
  ( Default Unpackspec rfs rfs,
    Default FromFields rfs r,
    Sel1 rfs (Field fi),
    DefaultFromField fi i,
    Reader env :> es,
    IOE :> es,
    eff ~ Eff es,
    StdHandler h eff,
    HaveTraits '[PathVar "id" i] ts,
    Sets h '[RequiredResponseHeader "Content-Type" Text, Body JSON (Rep i), Body JSON (Rep ())]
  ) =>
  (env -> Pool Connection) ->
  Table wfs rfs ->
  (i -> Field fi) ->
  (r -> wfs) ->
  [(Text, Text)] ->
  (RequestHandler h ts, SysOper')
dup envPool tbl idf r2t sqlErrArms =
  let handler = proc request -> do
        let tid = pick @(PathVar "id" i) $ from request
        tid_ <-
          arrM
            ( \tid -> catchSqlErrorRep sqlErrArms $ do
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
      sysOper = SysOper' "dup" (pack . show . tableIdentifier $ tbl) Nothing
   in (handler, sysOper)

upd ::
  forall i fi u wfs rfs d r eff env es h ts. -- (record id) (field id) update (table write fields) (table read fields) data (table record) eff effects (app env) arrow traits
  ( Sel1 rfs (Field fi),
    Default FromFields rfs r,
    Reader env :> es,
    IOE :> es,
    eff ~ Eff es,
    StdHandler h eff,
    HaveTraits '[PathVar "id" i] ts,
    Gets h '[Body JSON u],
    Sets h '[RequiredResponseHeader "Content-Type" Text, Body JSON (Rep d), Body JSON (Rep ())]
  ) =>
  (env -> Pool Connection) ->
  (u -> eff (Either Err u)) ->
  Table wfs rfs ->
  (i -> Field fi) ->
  (u -> (rfs -> wfs)) ->
  (r -> d) ->
  [(Text, Text)] ->
  (RequestHandler h ts, SysOper')
upd envPool chk tbl idf u2t r2d sqlErrArms =
  let handler = requestBody @u JSON errorHandler $
        proc request -> do
          let tid = pick @(PathVar "id" i) $ from request
          let toUpd = pick @(Body JSON u) $ from request
          ru <-
            arrM
              ( \(tid, toUpd) -> do
                  chkRes <- chk toUpd
                  catchSqlErrorRep sqlErrArms $ do
                    toUpd_ <- either throwError pure chkRes
                    pool <- asks envPool
                    ru <- liftIO . withResource pool $ \conn -> do
                      runUpdate
                        conn
                        Update
                          { uTable = tbl,
                            uUpdateWith = u2t toUpd_,
                            uWhere = (.== idf tid) . sel1,
                            uReturning = rReturning id
                          }
                    maybe (throwError $ Err 1 "update failure") (return . rep . r2d) (listToMaybe ru)
              )
              -<
                (tid, toUpd)
          respondA HTTP.ok200 JSON -< (ru :: Rep d)
      sysOper = SysOper' "upd" (pack . show . tableIdentifier $ tbl) Nothing
   in (handler, sysOper)

del ::
  forall i wfs rfs eff env es h ts. -- (record id) (table write fields) (table read fields) eff effects (app env) arrow traits
  ( Reader env :> es,
    IOE :> es,
    eff ~ Eff es,
    StdHandler h eff,
    HaveTraits '[PathVar "id" i] ts,
    Sets h '[RequiredResponseHeader "Content-Type" Text, Body JSON (Rep Text), Body JSON (Rep ())]
  ) =>
  (env -> Pool Connection) ->
  Table wfs rfs ->
  (i -> rfs -> Field SqlBool) ->
  (RequestHandler h ts, SysOper')
del envPool tbl con =
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
                          dWhere = con tid,
                          dReturning = rCount
                        }
                pure $ if c == 1 then repOk else repErr 1 "delete failure"
            )
            -<
              tid
        respondA HTTP.ok200 JSON -< (r :: Rep Text)
      sysOper = SysOper' "get" (pack . show . tableIdentifier $ tbl) Nothing
   in (handler, sysOper)
