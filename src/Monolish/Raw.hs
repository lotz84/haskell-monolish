{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Monolish.Raw where

import Foreign.Ptr
import Foreign.C.Types

import qualified Data.Vector.Storable.Mutable as VM
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exceptions as C
import qualified Language.C.Inline.Context as C

import Monolish.Raw.Types

C.context $ C.baseCtx <> C.vecCtx <> C.cppCtx <> mempty { C.ctxTypesTable = typeTable }

C.include "<monolish_blas.hpp>"
C.include "<monolish_vml.hpp>"

monolish_test :: IO ()
monolish_test =
  [C.throwBlock| void {
    monolish::vector<double> x(10, 0, 10);
    x.print_all();
  }|]

monolish_vector_double_vector_sizet_double :: CSize -> CDouble -> IO (Ptr (V CDouble))
monolish_vector_double_vector_sizet_double n value =
  [C.throwBlock| monolish::vector<double>* {
    return new monolish::vector<double>($(size_t n), $(double value));
  }|]

monolish_vector_double_vector_sizet_double_double :: CSize -> CDouble -> CDouble -> IO (Ptr (V CDouble))
monolish_vector_double_vector_sizet_double_double n min max =
  [C.throwBlock| monolish::vector<double>* {
    return new monolish::vector<double>($(size_t n), $(double min), $(double max));
  }|]

monolish_vector_double_destroy :: FunPtr (Ptr (V CDouble) -> IO ())
monolish_vector_double_destroy =
  [C.funPtr| void monolish_vector_double_destroy(monolish::vector<double>* object) { delete object; } |]

monolish_matrix_dense_double_destroy :: FunPtr (Ptr (M CDouble) -> IO ())
monolish_matrix_dense_double_destroy =
  [C.funPtr| void monolish_matrix_dense_double_destroy(monolish::matrix::Dense<double>* object) { delete object; } |]

monolish_blas_vecadd_vector_double_vector_double_vector_double :: Ptr (V CDouble) -> Ptr (V CDouble) -> IO (Ptr (V CDouble))
monolish_blas_vecadd_vector_double_vector_double_vector_double va vb =
  [C.throwBlock| monolish::vector<double>* {
    monolish::vector<double>* y = new monolish::vector<double>($(monolish::vector<double>* va)->size());
    monolish::blas::vecadd(*$(monolish::vector<double>* va), *$(monolish::vector<double>* vb), *y);
    return y;
  }|]

monolish_blas_vecsub_vector_double_vector_double_vector_double :: Ptr (V CDouble) -> Ptr (V CDouble) -> IO (Ptr (V CDouble))
monolish_blas_vecsub_vector_double_vector_double_vector_double va vb =
  [C.throwBlock| monolish::vector<double>* {
    monolish::vector<double>* y = new monolish::vector<double>($(monolish::vector<double>* va)->size());
    monolish::blas::vecsub(*$(monolish::vector<double>* va), *$(monolish::vector<double>* vb), *y);
    return y;
  }|]

monolish_blas_sum_vector_double :: Ptr (V CDouble) -> IO CDouble
monolish_blas_sum_vector_double v = [C.throwBlock| double { return monolish::blas::sum(*$(monolish::vector<double>* v)); }|]

monolish_vml_mul_vector_double_vector_double_vector_double :: Ptr (V CDouble) -> Ptr (V CDouble) -> IO (Ptr (V CDouble))
monolish_vml_mul_vector_double_vector_double_vector_double va vb =
  [C.throwBlock| monolish::vector<double>* {
    monolish::vector<double>* y = new monolish::vector<double>($(monolish::vector<double>* va)->size());
    monolish::vml::mul(*$(monolish::vector<double>* va), *$(monolish::vector<double>* vb), *y);
    return y;
  }|]

monolish_vml_max_vector_double_vector_double_vector_double :: Ptr (V CDouble) -> Ptr (V CDouble) -> IO (Ptr (V CDouble))
monolish_vml_max_vector_double_vector_double_vector_double va vb =
  [C.throwBlock| monolish::vector<double>* {
    monolish::vector<double>* y = new monolish::vector<double>($(monolish::vector<double>* va)->size());
    monolish::vml::max(*$(monolish::vector<double>* va), *$(monolish::vector<double>* vb), *y);
    return y;
  }|]

monolish_blas_scal_vector_double_double_vector_double :: CDouble -> Ptr (V CDouble) -> IO (Ptr (V CDouble))
monolish_blas_scal_vector_double_double_vector_double a x =
  [C.throwBlock| monolish::vector<double>* {
    monolish::vector<double>* y = new monolish::vector<double>($(monolish::vector<double>* x)->size());
    monolish::blas::copy(*$(monolish::vector<double>* x), *y);
    monolish::blas::scal($(double a), *y);
    return y;
  }|]

monolish_vector_double_print_all_bool :: Ptr (V CDouble) -> CBool -> IO ()
monolish_vector_double_print_all_bool v forceCpu =
  [C.throwBlock| void { $(monolish::vector<double>* v)->print_all($(bool forceCpu)); }|]

monolish_blas_dot_vector_double_vector_double :: Ptr (V CDouble) -> Ptr (V CDouble) -> IO CDouble
monolish_blas_dot_vector_double_vector_double va vb =
  [C.throwBlock| double {
    return monolish::blas::dot(*$(monolish::vector<double>* va), *$(monolish::vector<double>* vb));
  }|]

monolish_matrix_dense_double_sizet_sizet_double_double :: CSize -> CSize -> CDouble -> CDouble -> IO (Ptr (M CDouble))
monolish_matrix_dense_double_sizet_sizet_double_double m n min max =
  [C.throwBlock| monolish::matrix::Dense<double>* {
    return new monolish::matrix::Dense<double>($(size_t m), $(size_t n), $(double min), $(double max));
  }|]

monolish_matrix_dense_double_print_all_bool :: Ptr (M CDouble) -> CBool -> IO ()
monolish_matrix_dense_double_print_all_bool x forceCpu =
  [C.throwBlock| void { $(monolish::matrix::Dense<double>* x)->print_all($(bool forceCpu)); }|]

monolish_blas_matvec_dense_double_vector_double_vector_double :: Ptr (M CDouble) -> Ptr (V CDouble) -> IO (Ptr (V CDouble))
monolish_blas_matvec_dense_double_vector_double_vector_double a x =
  [C.throwBlock| monolish::vector<double>* {
    monolish::vector<double>* y = new monolish::vector<double>($(monolish::matrix::Dense<double>* a)->get_row());
    monolish::blas::matvec(*$(monolish::matrix::Dense<double>* a), *$(monolish::vector<double>* x), *y);
    return y;
  }|]

monolish_blas_matmul_dense_double_dense_double_dense_double :: Ptr (M CDouble) -> Ptr (M CDouble) -> IO (Ptr (M CDouble))
monolish_blas_matmul_dense_double_dense_double_dense_double a b =
  [C.throwBlock| monolish::matrix::Dense<double>* {
    monolish::matrix::Dense<double>* ans = new monolish::matrix::Dense<double>($(monolish::matrix::Dense<double>* a)->get_row(), $(monolish::matrix::Dense<double>* b)->get_col());
    monolish::blas::matmul(*$(monolish::matrix::Dense<double>* a), *$(monolish::matrix::Dense<double>* b), *ans);
    return ans;
  }|]

monolish_vml_matmul_dense_double_dense_double_dense_double :: Ptr (M CDouble) -> Ptr (M CDouble) -> IO (Ptr (M CDouble))
monolish_vml_matmul_dense_double_dense_double_dense_double a b =
  [C.throwBlock| monolish::matrix::Dense<double>* {
    monolish::matrix::Dense<double>* ans = new monolish::matrix::Dense<double>($(monolish::matrix::Dense<double>* a)->get_row(), $(monolish::matrix::Dense<double>* b)->get_col());
    monolish::vml::mul(*$(monolish::matrix::Dense<double>* a), *$(monolish::matrix::Dense<double>* b), *ans);
    return ans;
  }|]

monolish_matrix_dense_double_transpose :: Ptr (M CDouble) -> IO (Ptr (M CDouble))
monolish_matrix_dense_double_transpose a =
  [C.throwBlock| monolish::matrix::Dense<double>* {
    monolish::matrix::Dense<double>* y = new monolish::matrix::Dense<double>($(monolish::matrix::Dense<double>* a)->get_col(), $(monolish::matrix::Dense<double>* a)->get_row());
    y->transpose(*$(monolish::matrix::Dense<double>* a));
    return y;
  }|]

monolish_util_eye_int :: CInt -> IO (Ptr (M CDouble))
monolish_util_eye_int n =
  [C.throwBlock| monolish::matrix::Dense<double>* {
    monolish::matrix::Dense<double>* y = new monolish::matrix::Dense<double>($(int n), $(int n));
    y->convert(monolish::util::eye<double>($(int n)));
    return y;
  }|]

haskell_monolish_asrow_vector_double :: Ptr (V CDouble) -> IO (Ptr (M CDouble))
haskell_monolish_asrow_vector_double v =
  [C.throwBlock| monolish::matrix::Dense<double>* {
    size_t N = $(monolish::vector<double>* v)->size();
    monolish::matrix::Dense<double>* ans = new monolish::matrix::Dense<double>(1, N);
    double* a = $(monolish::vector<double>* v)->data() + $(monolish::vector<double>* v)->get_offset();
    double* y = ans->val.data();
    for (size_t i = 0; i < N; i++)
      y[i] = a[i];
    return ans;
  }|]

haskell_monolish_ascol_vector_double :: Ptr (V CDouble) -> IO (Ptr (M CDouble))
haskell_monolish_ascol_vector_double v =
  [C.throwBlock| monolish::matrix::Dense<double>* {
    size_t N = $(monolish::vector<double>* v)->size();
    monolish::matrix::Dense<double>* ans = new monolish::matrix::Dense<double>(N, 1);
    double* a = $(monolish::vector<double>* v)->data() + $(monolish::vector<double>* v)->get_offset();
    double* y = ans->val.data();
    for (size_t i = 0; i < N; i++)
      y[i] = a[i];
    return ans;
  }|]

haskell_monolish_abs_vector_double_vector_double :: Ptr (V CDouble) -> IO (Ptr (V CDouble))
haskell_monolish_abs_vector_double_vector_double v =
  [C.throwBlock| monolish::vector<double>* {
    size_t N = $(monolish::vector<double>* v)->size();
    monolish::vector<double>* ans = new monolish::vector<double>(N);
    double* a = $(monolish::vector<double>* v)->data() + $(monolish::vector<double>* v)->get_offset();
    double* y = ans->data() + ans->get_offset();
    for (size_t i = 0; i < N; i++)
      y[i] = std::abs(a[i]);
    return ans;
  }|]

haskell_monolish_signum_vector_double_vector_double :: Ptr (V CDouble) -> IO (Ptr (V CDouble))
haskell_monolish_signum_vector_double_vector_double v =
  [C.throwBlock| monolish::vector<double>* {
    size_t N = $(monolish::vector<double>* v)->size();
    monolish::vector<double>* ans = new monolish::vector<double>(N);
    double* a = $(monolish::vector<double>* v)->data() + $(monolish::vector<double>* v)->get_offset();
    double* y = ans->data() + ans->get_offset();
    for (size_t i = 0; i < N; i++)
      if (a[i] >= 0) {
        y[i] = 1;
      } else {
        y[i] = -1;
      }
    return ans;
  }|]

haskell_monolish_exp_vector_double_vector_double :: Ptr (V CDouble) -> IO (Ptr (V CDouble))
haskell_monolish_exp_vector_double_vector_double v =
  [C.throwBlock| monolish::vector<double>* {
    size_t N = $(monolish::vector<double>* v)->size();
    monolish::vector<double>* ans = new monolish::vector<double>(N);
    double* a = $(monolish::vector<double>* v)->data() + $(monolish::vector<double>* v)->get_offset();
    double* y = ans->data() + ans->get_offset();
    for (size_t i = 0; i < N; i++)
      y[i] = std::exp(a[i]);
    return ans;
  }|]

haskell_monolish_repeatrow_vector_double :: CInt -> Ptr (V CDouble) -> IO (Ptr (M CDouble))
haskell_monolish_repeatrow_vector_double n v =
  [C.throwBlock| monolish::matrix::Dense<double>* {
    size_t N = $(monolish::vector<double>* v)->size();
    monolish::matrix::Dense<double>* ans = new monolish::matrix::Dense<double>($(int n), N);
    double* a = $(monolish::vector<double>* v)->data() + $(monolish::vector<double>* v)->get_offset();
    double* y = ans->val.data();
    for (size_t i = 0; i < N * $(int n); i++)
      y[i] = a[i % N];
    return ans;
  }|]

haskell_monolish_repeatcol_vector_double :: CInt -> Ptr (V CDouble) -> IO (Ptr (M CDouble))
haskell_monolish_repeatcol_vector_double n v =
  [C.throwBlock| monolish::matrix::Dense<double>* {
    size_t N = $(monolish::vector<double>* v)->size();
    monolish::matrix::Dense<double>* ans = new monolish::matrix::Dense<double>(N, $(int n));
    double* a = $(monolish::vector<double>* v)->data() + $(monolish::vector<double>* v)->get_offset();
    double* y = ans->val.data();
    for (size_t i = 0; i < N * $(int n); i++)
      y[i] = a[i / $(int n)];
    return ans;
  }|]

haskell_monolish_maxelem_vector_double :: Ptr (V CDouble) -> IO CDouble
haskell_monolish_maxelem_vector_double v = [C.throwBlock| double { return monolish::vml::max(*$(monolish::vector<double>* v)); }|]

haskell_monolish_matadd_dense_double_dense_double :: Ptr (M CDouble) -> Ptr (M CDouble) -> IO (Ptr (M CDouble))
haskell_monolish_matadd_dense_double_dense_double a b =
  [C.throwBlock| monolish::matrix::Dense<double>* {
    monolish::matrix::Dense<double>* ans = new monolish::matrix::Dense<double>($(monolish::matrix::Dense<double>* a)->get_row(), $(monolish::matrix::Dense<double>* a)->get_col());
    monolish::vml::add(*$(monolish::matrix::Dense<double>* a), *$(monolish::matrix::Dense<double>* b), *ans);
    return ans;
  }|]

haskell_monolish_matsub_dense_double_dense_double :: Ptr (M CDouble) -> Ptr (M CDouble) -> IO (Ptr (M CDouble))
haskell_monolish_matsub_dense_double_dense_double a b =
  [C.throwBlock| monolish::matrix::Dense<double>* {
    monolish::matrix::Dense<double>* ans = new monolish::matrix::Dense<double>($(monolish::matrix::Dense<double>* a)->get_row(), $(monolish::matrix::Dense<double>* a)->get_col());
    monolish::vml::sub(*$(monolish::matrix::Dense<double>* a), *$(monolish::matrix::Dense<double>* b), *ans);
    return ans;
  }|]

haskell_monolish_fromlistv :: VM.IOVector CDouble -> IO (Ptr (V CDouble))
haskell_monolish_fromlistv vec =
  [C.throwBlock| monolish::vector<double>* {
    size_t N = $vec-len:vec;
    monolish::vector<double>* ans = new monolish::vector<double>(N);
    double* y = ans->data() + ans->get_offset();
    for (size_t i = 0; i < N; i++)
      y[i] = $vec-ptr:(double *vec)[i];
    return ans;
  }|]

haskell_monolish_fromlistm :: CInt -> CInt -> VM.IOVector CDouble -> IO (Ptr (M CDouble))
haskell_monolish_fromlistm m n vec =
  [C.throwBlock| monolish::matrix::Dense<double>* {
    size_t N = $vec-len:vec;
    monolish::matrix::Dense<double>* ans = new monolish::matrix::Dense<double>($(int m), $(int n));
    double* y = ans->val.data();
    for (size_t i = 0; i < $(int m) * $(int n); i++)
      y[i] = $vec-ptr:(double *vec)[i];
    return ans;
  }|]