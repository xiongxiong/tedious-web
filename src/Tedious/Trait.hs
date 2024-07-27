{-# LANGUAGE Arrows #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Tedious.Trait (
  RequestID (..),
  RequestIDValue (..),
  RequestIDMissing (..),
  requestID
) where

import Data.OpenApi (ApiKeyLocation (..), ApiKeyParams (ApiKeyParams, _apiKeyIn, _apiKeyName), SecurityScheme (..), SecuritySchemeType (SecuritySchemeApiKey))
import Data.Proxy (Proxy (..))
import Data.String (IsString (..))
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import WebGear.OpenApi (Absence, Attribute, Cookie, Existence (Optional), Get (..), HasTrait (..), OpenApiHandler (..), ParseStyle (Lenient), Prerequisite, QueryParam, Request, RequestHeader, Response, Set (..), Text, With, pick, returnA)
import WebGear.OpenApi.Trait.Auth (addSecurityScheme)
import WebGear.Server (ServerHandler)
import Control.Arrow (ArrowChoice)
import WebGear.Core (Middleware, probe)

data RequestID (location :: ApiKeyLocation) (name :: Symbol) = RequestID

newtype RequestIDValue = RequestIDValue Text

type instance Attribute (RequestID _ _) Request = RequestIDValue

data RequestIDMissing = RequestIDMissing

type instance Absence (RequestID _ _) = RequestIDMissing

type RequestIDQuery (name :: Symbol) = QueryParam Optional Lenient name Text

type RequestIDHeader (name :: Symbol) = RequestHeader Optional Lenient name Text

type RequestIDCookie (name :: Symbol) = Cookie Optional name Text

type instance Prerequisite (RequestID ApiKeyQuery name) ts = HasTrait (RequestIDQuery name) ts

type instance Prerequisite (RequestID ApiKeyHeader name) ts = HasTrait (RequestIDHeader name) ts

type instance Prerequisite (RequestID ApiKeyCookie name) ts = HasTrait (RequestIDCookie name) ts

instance (Monad m) => Get (ServerHandler m) (RequestID ApiKeyQuery name) where
  getTrait :: (HasTrait (RequestIDQuery name) ts) => RequestID ApiKeyQuery name -> ServerHandler m (Request `With` ts) (Either RequestIDMissing RequestIDValue)
  getTrait RequestID = proc request -> do
    let hdr = pick @(RequestIDQuery name) $ from request
    case hdr of
      Just (Right val) -> returnA -< Right (RequestIDValue val)
      _ -> returnA -< Left RequestIDMissing

instance (Monad m) => Get (ServerHandler m) (RequestID ApiKeyHeader name) where
  getTrait :: (HasTrait (RequestIDHeader name) ts) => RequestID ApiKeyHeader name -> ServerHandler m (Request `With` ts) (Either RequestIDMissing RequestIDValue)
  getTrait RequestID = proc request -> do
    let hdr = pick @(RequestIDHeader name) $ from request
    case hdr of
      Just (Right val) -> returnA -< Right (RequestIDValue val)
      _ -> returnA -< Left RequestIDMissing

instance (Monad m) => Get (ServerHandler m) (RequestID ApiKeyCookie name) where
  getTrait :: (HasTrait (RequestIDCookie name) ts) => RequestID ApiKeyCookie name -> ServerHandler m (Request `With` ts) (Either RequestIDMissing RequestIDValue)
  getTrait RequestID = proc request -> do
    let hdr = pick @(RequestIDCookie name) $ from request
    case hdr of
      Just val -> returnA -< Right (RequestIDValue val)
      _ -> returnA -< Left RequestIDMissing

instance (KnownSymbol name) => Get (OpenApiHandler m) (RequestID ApiKeyQuery name) where
  getTrait :: (r ~ RequestID ApiKeyQuery name) => r -> OpenApiHandler m (With Request ts) (Either (Absence r) (Attribute r Request))
  getTrait RequestID = OpenApiHandler $ addSecurityScheme requestIDName securityScheme
    where
      requestIDName = fromString $ symbolVal $ Proxy @name
      securityScheme = SecurityScheme {_securitySchemeType = SecuritySchemeApiKey ApiKeyParams {_apiKeyName = requestIDName, _apiKeyIn = ApiKeyQuery}, _securitySchemeDescription = Nothing}

instance (KnownSymbol name) => Get (OpenApiHandler m) (RequestID ApiKeyHeader name) where
  getTrait :: (r ~ RequestID ApiKeyHeader name) => r -> OpenApiHandler m (With Request ts) (Either (Absence r) (Attribute r Request))
  getTrait RequestID = OpenApiHandler $ addSecurityScheme requestIDName securityScheme
    where
      requestIDName = fromString $ symbolVal $ Proxy @name
      securityScheme = SecurityScheme {_securitySchemeType = SecuritySchemeApiKey ApiKeyParams {_apiKeyName = requestIDName, _apiKeyIn = ApiKeyHeader}, _securitySchemeDescription = Nothing}

instance (KnownSymbol name) => Get (OpenApiHandler m) (RequestID ApiKeyCookie name) where
  getTrait :: (r ~ RequestID ApiKeyCookie name) => r -> OpenApiHandler m (With Request ts) (Either (Absence r) (Attribute r Request))
  getTrait RequestID = OpenApiHandler $ addSecurityScheme requestIDName securityScheme
    where
      requestIDName = fromString $ symbolVal $ Proxy @name
      securityScheme = SecurityScheme {_securitySchemeType = SecuritySchemeApiKey ApiKeyParams {_apiKeyName = requestIDName, _apiKeyIn = ApiKeyCookie}, _securitySchemeDescription = Nothing}

instance Set (OpenApiHandler m) (RequestID ApiKeyCookie name) where
  setTrait ::
    RequestID ApiKeyCookie name ->
    (With Response ts -> Response -> Attribute (RequestID ApiKeyCookie name) Response -> With Response (RequestID ApiKeyCookie name : ts)) ->
    OpenApiHandler m (With Response ts, Attribute (RequestID ApiKeyCookie name) Response) (With Response (RequestID ApiKeyCookie name : ts))
  setTrait RequestID _ = OpenApiHandler pure

requestID :: (Get h r, r ~ RequestID (location :: ApiKeyLocation) name, ArrowChoice h, Prerequisite r ts) => h (Request `With` ts, RequestIDMissing) Response -> Middleware h ts (r : ts)
requestID errorHandler nextHandler =
  proc request -> do
    result <- probe RequestID -< request
    case result of
      Left e -> errorHandler -< (request, e)
      Right request' -> nextHandler -< request'
