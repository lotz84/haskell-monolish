{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Monolish.Safe where

import Data.Coerce (coerce)
import Data.Proxy
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Storable (Storable(..))
import Foreign.Ptr
import GHC.ForeignPtr (mallocPlainForeignPtrBytes)
import GHC.TypeLits

import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VM

import Monolish.Raw
import Monolish.Raw.Types
import Monolish.Types


constVector :: forall n. KnownNat n => Double -> IO (Vector n)
constVector value = do
  let size = fromIntegral $ natVal (Proxy @n)
  v <- monolish_vector_double_vector_sizet_double size (coerce value)
  fptr <- newForeignPtr monolish_vector_double_destroy v
  pure (Vector fptr)

constMatrix :: forall m n. (KnownNat m, KnownNat n) => Double -> IO (Matrix m n)
constMatrix value = do
  let nRow = fromIntegral $ natVal (Proxy @m)
      nCol = fromIntegral $ natVal (Proxy @n)
  m <- monolish_matrix_dense_double_sizet_sizet_double nRow nCol (coerce value)
  fptr <- newForeignPtr monolish_matrix_dense_double_destroy m
  pure (Matrix fptr)

liftVector1 :: (Ptr (V CDouble) -> IO (Ptr (V CDouble)))
            -> Vector n -> IO (Vector n)
liftVector1 f (Vector v) =
  withForeignPtr v \ptr ->
    f ptr >>= \ans ->
      fmap Vector (newForeignPtr monolish_vector_double_destroy ans)

liftVector2 :: (Ptr (V CDouble) -> Ptr (V CDouble) -> IO (Ptr (V CDouble)))
            -> Vector n -> Vector n -> IO (Vector n)
liftVector2 f (Vector va) (Vector vb) =
  withForeignPtr va \ptra ->
    withForeignPtr vb \ptrb ->
      f ptra ptrb >>= \ans ->
        fmap Vector (newForeignPtr monolish_vector_double_destroy ans)


vecadd :: Vector n -> Vector n -> IO (Vector n)
vecadd = liftVector2 monolish_blas_vecadd_vector_double_vector_double_vector_double

vecsub :: Vector n -> Vector n -> IO (Vector n)
vecsub = liftVector2 monolish_blas_vecsub_vector_double_vector_double_vector_double

mul :: Vector n -> Vector n -> IO (Vector n)
mul = liftVector2 monolish_vml_mul_vector_double_vector_double_vector_double

dot :: Vector n -> Vector n -> IO Double
dot (Vector va) (Vector vb) =
  withForeignPtr va \ptra ->
    withForeignPtr vb \ptrb ->
      coerce <$> monolish_blas_dot_vector_double_vector_double ptra ptrb

abs :: Vector n -> IO (Vector n)
abs = liftVector1 haskell_monolish_abs_vector_double_vector_double

signum :: Vector n -> IO (Vector n)
signum = liftVector1 haskell_monolish_signum_vector_double_vector_double

max :: Vector n -> Vector n -> IO (Vector n)
max = liftVector2 monolish_vml_max_vector_double_vector_double_vector_double

scal :: Double -> Vector n -> IO (Vector n)
scal a = liftVector1 (monolish_blas_scal_vector_double_double_vector_double (coerce a))

exp :: Vector n -> IO (Vector n)
exp = liftVector1 haskell_monolish_exp_vector_double_vector_double

sum :: Vector n -> IO Double
sum (Vector x) = withForeignPtr x \ptr -> fmap coerce (monolish_blas_sum_vector_double ptr)

eye :: forall n. KnownNat n => IO (Matrix n n)
eye = do
  let size = fromIntegral $ natVal (Proxy @n)
  m <- monolish_util_eye_int size
  fptr <- newForeignPtr monolish_matrix_dense_double_destroy m
  pure (Matrix fptr)

randomMatrix :: forall m n. (KnownNat m, KnownNat n) => Double -> Double -> IO (Matrix m n)
randomMatrix min max = do
  let nRow = fromIntegral $ natVal (Proxy @m)
      nCol = fromIntegral $ natVal (Proxy @n)
  m <- monolish_matrix_dense_double_sizet_sizet_double_double nRow nCol (coerce min) (coerce max)
  fptr <- newForeignPtr monolish_matrix_dense_double_destroy m
  pure (Matrix fptr)

printVector :: Vector n -> IO ()
printVector (Vector fptr) = withForeignPtr fptr \ptr -> monolish_vector_double_print_all_bool ptr (fromIntegral 0)

printMatrix :: Matrix m n -> IO ()
printMatrix (Matrix fptr) = withForeignPtr fptr \ptr -> monolish_matrix_dense_double_print_all_bool ptr (fromIntegral 0)

randomVector :: forall n. KnownNat n => Double -> Double -> IO (Vector n)
randomVector min max = do
  let size = fromIntegral $ natVal (Proxy @n)
  ptr <- monolish_vector_double_vector_sizet_double_double size (coerce min) (coerce max)
  fmap Vector $ newForeignPtr monolish_vector_double_destroy ptr

infixr 8 #>
(#>) :: forall m n. Matrix m n -> Vector n -> IO (Vector m)
(Matrix a) #> (Vector x) =
  withForeignPtr a \pa ->
    withForeignPtr x \px ->
      monolish_blas_matvec_dense_double_vector_double_vector_double pa px >>= \ans ->
        fmap Vector (newForeignPtr monolish_vector_double_destroy ans)

transpose :: forall m n. Matrix m n -> IO (Matrix n m)
transpose (Matrix a) =
  withForeignPtr a \pa ->
    monolish_matrix_dense_double_transpose pa >>= \ans ->
      fmap Matrix (newForeignPtr monolish_matrix_dense_double_destroy ans)

matabs :: forall m n. Matrix m n -> IO (Matrix m n)
matabs (Matrix a) =
  withForeignPtr a \pa ->
    haskell_monolish_matabs pa >>= \ans ->
      fmap Matrix (newForeignPtr monolish_matrix_dense_double_destroy ans)

matsignum :: forall m n. Matrix m n -> IO (Matrix m n)
matsignum (Matrix a) =
  withForeignPtr a \pa ->
    haskell_monolish_matsignum pa >>= \ans ->
      fmap Matrix (newForeignPtr monolish_matrix_dense_double_destroy ans)

asRow :: forall n. Vector n -> IO (Matrix 1 n)
asRow (Vector x) =
  withForeignPtr x \px ->
    haskell_monolish_asrow_vector_double px >>= \ans ->
      fmap Matrix (newForeignPtr monolish_matrix_dense_double_destroy ans)

asCol :: forall n. Vector n -> IO (Matrix n 1)
asCol (Vector x) =
  withForeignPtr x \px ->
    haskell_monolish_ascol_vector_double px >>= \ans ->
      fmap Matrix (newForeignPtr monolish_matrix_dense_double_destroy ans)

matmul :: Matrix m k -> Matrix k n -> IO (Matrix m n)
matmul (Matrix a) (Matrix b) =
  withForeignPtr a \pa ->
    withForeignPtr b \pb ->
      monolish_blas_matmul_dense_double_dense_double_dense_double pa pb >>= \ans ->
        fmap Matrix (newForeignPtr monolish_matrix_dense_double_destroy ans)

hadamard :: Matrix m n -> Matrix m n -> IO (Matrix m n)
hadamard (Matrix a) (Matrix b) =
  withForeignPtr a \pa ->
    withForeignPtr b \pb ->
      monolish_vml_matmul_dense_double_dense_double_dense_double pa pb >>= \ans ->
        fmap Matrix (newForeignPtr monolish_matrix_dense_double_destroy ans)

repeatRow :: forall m n. KnownNat m => Vector n -> IO (Matrix m n)
repeatRow (Vector v) =
  let size = fromIntegral $ natVal (Proxy @m)
   in withForeignPtr v \p -> do
        m <- haskell_monolish_repeatrow_vector_double size p
        fptr <- newForeignPtr monolish_matrix_dense_double_destroy m
        pure (Matrix fptr)

repeatCol :: forall m n. KnownNat n => Vector m -> IO (Matrix m n)
repeatCol (Vector v) =
  let size = fromIntegral $ natVal (Proxy @n)
   in withForeignPtr v \p -> do
        m <- haskell_monolish_repeatcol_vector_double size p
        fptr <- newForeignPtr monolish_matrix_dense_double_destroy m
        pure (Matrix fptr)

matadd :: Matrix m n -> Matrix m n -> IO (Matrix m n)
matadd (Matrix a) (Matrix b) =
  withForeignPtr a \pa ->
    withForeignPtr b \pb ->
      haskell_monolish_matadd_dense_double_dense_double pa pb >>= \ans ->
        fmap Matrix (newForeignPtr monolish_matrix_dense_double_destroy ans)

matsub :: Matrix m n -> Matrix m n -> IO (Matrix m n)
matsub (Matrix a) (Matrix b) =
  withForeignPtr a \pa ->
    withForeignPtr b \pb ->
      haskell_monolish_matsub_dense_double_dense_double pa pb >>= \ans ->
        fmap Matrix (newForeignPtr monolish_matrix_dense_double_destroy ans)

maxElem :: Vector n -> IO Double
maxElem (Vector v) =
  withForeignPtr v \pv ->
    fmap coerce (haskell_monolish_maxelem_vector_double pv)

fromListV :: forall n. KnownNat n => [Double] -> IO (Vector n)
fromListV = fromVector . VS.fromList

fromVector :: forall n. KnownNat n => VS.Vector Double -> IO (Vector n)
fromVector vec = do
  ans <- haskell_monolish_fromlistv =<< VS.thaw (VS.map coerce vec)
  fptr <- (newForeignPtr monolish_vector_double_destroy ans)
  pure (Vector fptr)

createArrayForeignPtr :: Storable a => Int -> IO (ForeignPtr a)
createArrayForeignPtr n = doMalloc undefined
  where
  doMalloc :: Storable a => a -> IO (ForeignPtr a)
  doMalloc dummy = mallocPlainForeignPtrBytes (n * sizeOf dummy)

toVector :: forall n. KnownNat n => Vector n -> IO (VS.Vector Double)
toVector (Vector vec) = do
  let size :: Num a => a
      size = fromIntegral $ natVal (Proxy @n)
  fptr <- createArrayForeignPtr size
  withForeignPtr vec $ \vptr ->
    withForeignPtr fptr $ \ptr ->
      haskell_monolish_vector_copy size vptr ptr
  pure . VS.map coerce $ VS.unsafeFromForeignPtr fptr 0 size

toList :: KnownNat n => Vector n -> IO [Double]
toList vec = VS.toList <$> toVector vec

fromListM :: forall m n. (KnownNat m, KnownNat n) => [Double] -> IO (Matrix m n)
fromListM xs = do
  let height = fromIntegral $ natVal (Proxy @m)
      width  = fromIntegral $ natVal (Proxy @n)
  vec <- VS.thaw (VS.map coerce $ VS.fromList xs)
  ans <- haskell_monolish_fromlistm height width vec
  fptr <- newForeignPtr monolish_matrix_dense_double_destroy ans
  pure (Matrix fptr)