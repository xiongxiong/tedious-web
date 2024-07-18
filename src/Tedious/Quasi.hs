module Tedious.Quasi (
  tedious
) where

import Tedious.Parser (decTedious)
import Language.Haskell.TH.Quote (QuasiQuoter (..))

tedious :: QuasiQuoter
tedious =
  QuasiQuoter
    { quoteExp = error "tedious cannot be used as exp",
      quotePat = error "tedious cannot be used as pat",
      quoteType = error "tedious cannot be used as type",
      quoteDec = decTedious
    }