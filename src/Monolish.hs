{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

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
  hadamard,
  repeatRow,
  repeatCol,
  matadd,
  matsub,
  fromListV,
  randomVector',
  fromListM,
  randomMatrix',
  dot,
  module Monolish.Safe
) where

import Prelude hiding (max, exp, sum)

import Control.Monad
import Data.Proxy
import Foreign.Ptr
import Foreign.ForeignPtr
import GHC.TypeLits
import System.IO.Unsafe

import System.Random

import Monolish.Raw
import Monolish.Safe (printMatrix, printVector, randomMatrix, randomVector)
import qualified Monolish.Safe as Safe
import Monolish.Types


instance KnownNat n => Num (Vector n) where
  x + y = unsafePerformIO $ Safe.vecadd x y
  x * y = unsafePerformIO $ Safe.mul x y
  abs x = unsafePerformIO $ Safe.abs x
  signum x = unsafePerformIO $ Safe.signum x
  fromInteger x = constVector (fromIntegral x)
  x - y = unsafePerformIO $ Safe.vecsub x y

constVector :: forall n. KnownNat n => Double -> Vector n
constVector = unsafePerformIO . Safe.constVector

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

matadd :: Matrix m n -> Matrix m n -> Matrix m n
matadd a b = unsafePerformIO $ Safe.matadd a b

matsub :: Matrix m n -> Matrix m n -> Matrix m n
matsub a b = unsafePerformIO $ Safe.matsub a b

fromListV :: KnownNat n => [Double] -> Vector n
fromListV = unsafePerformIO . Safe.fromListV

randomVector' :: forall n. KnownNat n => Double -> Double -> IO (Vector n)
randomVector' min max =
  let size = fromIntegral $ natVal (Proxy @n)
   in fmap Monolish.fromListV $ replicateM size (randomRIO (min, max))

fromListM :: forall m n. (KnownNat m, KnownNat n) => [Double] -> Matrix m n
fromListM = unsafePerformIO . Safe.fromListM

randomMatrix' :: forall m n. (KnownNat m, KnownNat n) => Double -> Double -> IO (Matrix m n)
randomMatrix' min max =
  let height = fromIntegral $ natVal (Proxy @m)
      width  = fromIntegral $ natVal (Proxy @n)
   in fmap Monolish.fromListM $ replicateM (height * width) (randomRIO (min, max))