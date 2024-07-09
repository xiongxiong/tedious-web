{-# LANGUAGE KindSignatures #-}

module Tedious.Orphan where

import Control.Lens ((?~))
import Data.Default (Default (..))
import Data.Function ((&))
import Data.OpenApi (HasTitle (..), ToSchema, declareSchema)
import Data.OpenApi.Internal.Schema (unnamed)
import Data.OpenApi.Schema (ToSchema (..))
import Data.Proxy (Proxy (..))
import Data.Tagged (Tagged)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TLB
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Typeable (typeRep)
import GHC.Base (Symbol)
import GHC.TypeLits (KnownSymbol, Natural)

instance forall s b. (KnownSymbol s, ToSchema b) => ToSchema (Tagged (s :: Symbol) b) where
  declareNamedSchema _ = do
    _schema <- declareSchema (Proxy :: Proxy b)
    return . unnamed $ _schema & title ?~ (T.pack . init . tail . show . typeRep $ (Proxy :: Proxy s))

instance Default T.Text where
  def = T.empty

instance Default TL.Text where
  def = TL.empty

instance Default TLB.Builder where
  def = mempty

instance Default Bool where
  def = False

instance Default Natural where
  def = 0

instance Default UTCTime where
  def = posixSecondsToUTCTime 0