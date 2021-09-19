{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module DNN where

import GHC.TypeLits
import System.IO

import qualified Data.Vector as V
import Lens.Micro

import Monolish ((#>))
import qualified Monolish as Monolish
import Monolish.Types

type ParaLens p p' a a' b b' = Lens (p, a) (p', a') b b'

type ParaLens' p a b = ParaLens p p a a b b

(%) :: ParaLens' p a b -> ParaLens' q b c -> ParaLens' (p, q) a c
(%) l1 l2 = lens view update
  where
  view   ((p, q), a)   = (q, (p, a) ^. l1) ^. l2
  update ((p, q), a) c =
    let (q', b )  = (q, (p, a) ^. l1) & l2 .~ c
        (p', a') = (p, a) & l1 .~ b
     in ((p', q'), a')

linear :: (KnownNat m, KnownNat n) => ParaLens' (Matrix m n) (Vector n) (Vector m)
linear = lens view update
  where
  view   (w, x)   = w #> x
  update (w, x) y = (Monolish.outer y x, Monolish.transpose w #> y)

bias :: KnownNat n => ParaLens' (Vector n) (Vector n) (Vector n)
bias = lens view update
  where
  view   (b, x)   = x + b
  update (b, x) y = (y, y)

relu :: KnownNat n => ParaLens' () (Vector n) (Vector n)
relu = lens view update
  where
  view   ((), x)   = Monolish.max (Monolish.constVector 0) x
  update ((), x) y = ((), y * (Monolish.scal 0.5 (signum x + Monolish.constVector 1)))

softmax :: KnownNat n => ParaLens' () (Vector n) (Vector n)
softmax = lens view update
  where
  view ((), x) =
    let xMax = Monolish.maxElem x
        expX = Monolish.exp (x - Monolish.constVector xMax)
        denom = Monolish.sum expX
     in Monolish.scal (1/denom) expX
  update ((), x) y =
    let z = view ((), x)
     in ((), (Monolish.repeatCol z * (Monolish.eye - Monolish.repeatRow z)) #> y)

crossEntropyLoss :: KnownNat n => ParaLens' (Vector n) (Vector n) Double
crossEntropyLoss = lens view update
  where
  view (y', y) = log (Monolish.sum $ Monolish.exp y) - (Monolish.sum $ y' * y)
  update (y', y) z =
    let expY = Monolish.exp y
     in (Monolish.scal z (-y), Monolish.scal z (Monolish.scal (1 / Monolish.sum expY) expY - y'))

learningRate :: ParaLens' () Double ()
learningRate = lens (const ()) update
  where
  update ((), loss) () = ((), (-0.001) * loss)

class Parameter a where
  update :: a -> a -> a

instance Parameter () where
  update () () = ()

instance KnownNat n => Parameter (Vector n) where
  update !v !w = v + w

instance (KnownNat m, KnownNat n) => Parameter (Matrix m n) where
  update !a !b = a + b

instance (Parameter a, Parameter b) => Parameter (a, b) where
  update (!a, !b) (!a', !b') = (update a a', update b b')

updateParam :: (Parameter p, KnownNat n) => ParaLens' p a (Vector n) -> a -> Vector n -> p -> p
updateParam model a b p =
  let l = model % crossEntropyLoss % learningRate
      (((p', _), ()), _) = (((p, b), ()), a) & l .~ ()
   in update p p'

train :: (Parameter p, KnownNat n) => ParaLens' p a (Vector n) -> [(a, Vector n)] -> p -> p
train model dataset init = foldl (\p (a, b) -> updateParam model a b p) init dataset

trainV :: (Parameter p, KnownNat n) => ParaLens' p a (Vector n) -> V.Vector (a, Vector n) -> p -> p
trainV model dataset init = V.foldl' (\p (a, b) -> updateParam model a b p) init dataset

accuracyV :: (Parameter p, KnownNat n) => ParaLens' p a (Vector n) -> V.Vector (a, Vector n) -> p -> Double
accuracyV model dataset p = V.foldl' (\count (a, b) -> count + if ((p, b), a) ^. (model % crossEntropyLoss) <= (-log 0.5) then 1.0 else 0.0) 0.0 dataset / fromIntegral (V.length dataset)

eval :: ParaLens' p a b -> p -> a -> b
eval model params input = (params, input) ^. model