{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE Arrows #-}

module Tedious.Handler where

import Control.Lens ((^.))
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Pool (Pool, withResource)
import Data.Profunctor.Product.Default (Default)
import Data.Text (Text, pack)
import Data.Tuple.All (Sel1 (..))
import Database.PostgreSQL.Simple (Connection)
import Effectful (Eff, IOE, MonadIO (..), (:>))
import Effectful.Error.Dynamic (throwError)
import Effectful.Reader.Dynamic (Reader, ask)
import qualified Network.HTTP.Types as HTTP
import Opaleye (DefaultFromField, Delete (Delete, dReturning, dTable, dWhere), Field, FromFields, Insert (Insert, iOnConflict, iReturning, iRows, iTable), Order, SqlBool, SqlInt8, Table, Unpackspec, Update (Update, uReturning, uTable, uUpdateWith, uWhere), countRows, limit, offset, orderBy, rCount, rReturning, runDelete, runInsert, runSelect, runUpdate, selectTable, where_, (.==))
import Tedious.Entity (Err (Err), Page (Page), PageI (_pageIFilter, _pageIPage), PageO (PageO), Rep, catchRep, fillPage, pageIndex, pageSize, rep, repErr, repOk)
import WebGear.Core (Body, Description, Gets, Handler (arrM, setDescription, setSummary), HasTrait (from), HaveTraits, JSON (JSON), Middleware, PathVar, Request, RequestHandler, RequiredResponseHeader, Response, Sets, StdHandler, Summary, With, pick, requestBody, respondA, (<<<))

withDoc :: (Handler h m) => Summary -> Description -> Middleware h ts ts
withDoc summ descr handler = setDescription descr <<< setSummary summ <<< handler

errorHandler ::
  ( Show e,
    StdHandler h m,
    Sets h '[RequiredResponseHeader "Content-Type" Text, Body JSON (Rep ())]
  ) =>
  h (Request `With` ts, e) Response
errorHandler = proc (_, err) -> respondA HTTP.ok200 JSON -< (repErr 1 (pack . show $ err) :: Rep ())

list ::
  forall i d f ids t fs r h ts es. -- (record id) data (data filter) [id] table (table fields) (table record) arrow traits effects
  ( ids ~ [i],
    Integral i,
    DefaultFromField SqlInt8 i,
    Default Unpackspec fs fs,
    Default FromFields fs r,
    Reader (Pool Connection) :> es,
    IOE :> es,
    StdHandler h (Eff es),
    Gets h '[Body JSON (PageI f)],
    Sets h '[RequiredResponseHeader "Content-Type" Text, Body JSON (Rep (PageO [d])), Body JSON (Rep ())]
  ) =>
  Table t fs ->
  (Maybe f -> fs -> Field SqlBool) ->
  Order fs ->
  (r -> d) ->
  RequestHandler h ts
list tbl flt ord r2d =
  requestBody @(PageI f) JSON errorHandler $
    proc request -> do
      let pageI = pick @(Body JSON (PageI f)) $ from request
      res <-
        arrM
          ( \pageI -> catchRep $ do
              let mPage = _pageIPage pageI
              pool <- ask
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

get ::
  forall i fi d t fs r h ts es. -- (record id) (field id) data table (table fields) (table record) arrow traits effects
  ( Default Unpackspec fs fs,
    Default FromFields fs r,
    Sel1 fs (Field fi),
    Reader (Pool Connection) :> es,
    IOE :> es,
    StdHandler h (Eff es),
    HaveTraits '[PathVar "id" i] ts,
    Sets h '[RequiredResponseHeader "Content-Type" Text, Body JSON (Rep (Maybe d))]
  ) =>
  Table t fs ->
  (i -> Field fi) ->
  (r -> d) ->
  RequestHandler h ts
get tbl idf r2d =
  proc request -> do
    let tid = pick @(PathVar "id" i) $ from request
    (md :: Maybe d) <-
      arrM
        ( \tid -> do
            pool <- ask
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

add ::
  forall i fi a t fs h ts es. -- (record id) (data to add) table (table fields) arrow traits effects
  ( Sel1 fs (Field fi),
    DefaultFromField fi i,
    Reader (Pool Connection) :> es,
    IOE :> es,
    StdHandler h (Eff es),
    Gets h '[Body JSON a],
    Sets h '[RequiredResponseHeader "Content-Type" Text, Body JSON (Rep i), Body JSON (Rep ())]
  ) =>
  Table t fs ->
  (a -> [t]) ->
  RequestHandler h ts
add tbl a2t =
  requestBody @a JSON errorHandler $
    proc request -> do
      let toAdd = pick @(Body JSON a) $ from request
      tid <-
        arrM
          ( \toAdd -> catchRep $ do
              pool <- ask
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

upd ::
  forall i fi u t fs d r h ts es. -- (record id) (field id) update table (table fields) data (table record) arrow traits effects
  ( Sel1 fs (Field fi),
    Default FromFields fs r,
    Reader (Pool Connection) :> es,
    IOE :> es,
    StdHandler h (Eff es),
    HaveTraits '[PathVar "id" i] ts,
    Gets h '[Body JSON u],
    Sets h '[RequiredResponseHeader "Content-Type" Text, Body JSON (Rep d), Body JSON (Rep ())]
  ) =>
  Table t fs ->
  (i -> Field fi) ->
  (u -> (fs -> t)) ->
  (r -> d) ->
  RequestHandler h ts
upd tbl idf u2t r2d =
  requestBody @u JSON errorHandler $
    proc request -> do
      let tid = pick @(PathVar "id" i) $ from request
      let toUpd = pick @(Body JSON u) $ from request
      ru <-
        arrM
          ( \(tid, toUpd) -> catchRep $ do
              pool <- ask
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

del ::
  forall i fi t fs h ts es. -- (record id) (field id) table (table fields) data (table fields) arrow traits effects
  ( Sel1 fs (Field fi),
    Reader (Pool Connection) :> es,
    IOE :> es,
    StdHandler h (Eff es),
    HaveTraits '[PathVar "id" i] ts,
    Sets h '[RequiredResponseHeader "Content-Type" Text, Body JSON (Rep Text), Body JSON (Rep ())]
  ) =>
  Table t fs ->
  (i -> Field fi) ->
  RequestHandler h ts
del tbl idf =
  proc request -> do
    let tid = pick @(PathVar "id" i) $ from request
    r <-
      arrM
        ( \tid -> do
            pool <- ask
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