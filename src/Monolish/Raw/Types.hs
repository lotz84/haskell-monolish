{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Monolish.Raw.Types where

import qualified Data.Map as Map
import qualified Language.C.Types as C
import qualified Language.Haskell.TH.Lib as TH

data V a
data M a

typeTable :: Map.Map C.TypeSpecifier TH.TypeQ
typeTable = Map.fromList [ (C.TypeName "monolish::vector", [t|V|])
                         , (C.TypeName "monolish::matrix::Dense", [t|M|])
                         ]