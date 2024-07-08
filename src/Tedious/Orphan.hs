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
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
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

instance Default Natural where
  def = 0