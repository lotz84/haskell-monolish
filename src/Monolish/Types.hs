{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}

module Monolish.Types where

import Foreign.C.Types
import Foreign.ForeignPtr
import GHC.TypeLits

import Monolish.Raw
import Monolish.Raw.Types

newtype Vector (n :: Nat)            = Vector (ForeignPtr (V CDouble))
newtype Matrix (m :: Nat) (n :: Nat) = Matrix (ForeignPtr (M CDouble))