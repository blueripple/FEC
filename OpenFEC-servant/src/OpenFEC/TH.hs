{-# LANGUAGE TemplateHaskell #-}
module OpenFEC.TH where

import           Data.Lens.Aeson
import           Language.Haskell.TH

typeToAesonLens :: Type -> Prism Value a
typeToAesonLens
