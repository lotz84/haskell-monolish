{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Monolish (
  constVector,
  max,
  scal,
  (#>),
  transpose,
  asRow,
  asCol,
  matmul,
  outer,
  maxElem,
  exp,
  sum,
  eye,
  repeatRow,
  repeatCol,
  fromListV,
  fromVector,
  fromListM,
  dot,
  module Monolish.Safe
) where

import Prelude hiding (max, exp, sum)

import Control.Monad
import Data.Proxy
import Foreign.Ptr
import Foreign.ForeignPtr
import GHC.Exts (IsList(..))
import GHC.TypeLits
import System.IO.Unsafe

import qualified Data.Vector.Storable as V

import Monolish.Raw
import Monolish.Safe (printMatrix, printVector, randomMatrix, randomVector)
import qualified Monolish.Safe as Safe
import Monolish.Types

instance KnownNat n => Num (Vector n) where
  x + y  = unsafePerformIO $ Safe.vecadd x y
  x * y  = unsafePerformIO $ Safe.mul x y
  x - y  = unsafePerformIO $ Safe.vecsub x y
  abs    = unsafePerformIO . Safe.abs
  signum = unsafePerformIO . Safe.signum
  fromInteger = constVector . fromIntegral

instance KnownNat n => IsList (Vector n) where
  type Item (Vector n) = Double
  fromList = fromListV
  toList = unsafePerformIO . Safe.toList

instance (KnownNat m, KnownNat n) => Num (Matrix m n) where
  x + y  = unsafePerformIO $ Safe.matadd x y
  x * y  = unsafePerformIO $ Safe.hadamard x y
  x - y  = unsafePerformIO $ Safe.matsub x y
  abs    = unsafePerformIO . Safe.matabs
  signum = unsafePerformIO . Safe.matsignum
  fromInteger = constMatrix . fromIntegral

constVector :: forall n. KnownNat n => Double -> Vector n
constVector = unsafePerformIO . Safe.constVector

constMatrix :: forall m n. (KnownNat m, KnownNat n) => Double -> Matrix m n
constMatrix = unsafePerformIO . Safe.constMatrix

max :: Vector n -> Vector n -> Vector n
max x y = unsafePerformIO $ Safe.max x y

exp :: Vector n -> Vector n
exp = unsafePerformIO . Safe.exp

sum :: Vector n -> Double
sum = unsafePerformIO . Safe.sum

scal :: Double -> Vector n -> Vector n
scal a x = unsafePerformIO $ Safe.scal a x

(#>) :: forall m n. (KnownNat m, KnownNat n) => Matrix m n -> Vector n -> (Vector m)
a #> x = unsafePerformIO $ a Safe.#> x

transpose :: forall m n. Matrix m n -> Matrix n m
transpose = unsafePerformIO . Safe.transpose

asRow :: forall n. Vector n -> Matrix 1 n
asRow = unsafePerformIO . Safe.asRow

asCol :: forall n. Vector n -> Matrix n 1
asCol = unsafePerformIO . Safe.asCol

matmul :: Matrix m k -> Matrix k n -> Matrix m n
matmul a b = unsafePerformIO $ Safe.matmul a b

dot :: Vector n -> Vector n -> Double
dot a b = unsafePerformIO $ Safe.dot a b

outer :: Vector m -> Vector n -> Matrix m n
outer v w = asCol v `matmul` asRow w

maxElem :: Vector n -> Double
maxElem = unsafePerformIO . Safe.maxElem

eye :: KnownNat n => Matrix n n
eye = unsafePerformIO $ Safe.eye

hadamard :: Matrix m n -> Matrix m n -> Matrix m n
hadamard a b = unsafePerformIO $ Safe.hadamard a b

repeatRow :: KnownNat m => Vector n -> Matrix m n
repeatRow = unsafePerformIO . Safe.repeatRow

repeatCol :: KnownNat n => Vector m -> Matrix m n
repeatCol = unsafePerformIO . Safe.repeatCol

fromListV :: KnownNat n => [Double] -> Vector n
fromListV = unsafePerformIO . Safe.fromListV

fromVector :: KnownNat n => V.Vector Double -> Vector n
fromVector = unsafePerformIO . Safe.fromVector

fromListM :: forall m n. (KnownNat m, KnownNat n) => [Double] -> Matrix m n
fromListM = unsafePerformIO . Safe.fromListM