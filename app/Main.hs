{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BangPatterns #-}

module Main where

import Data.Function (fix)
import GHC.TypeLits
import System.IO

import qualified Data.Vector as V
import Lens.Micro

import MNIST as MNIST
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
     in ((), Monolish.repeatCol z `Monolish.hadamard` (Monolish.eye `Monolish.matsub` Monolish.repeatRow z) #> y)

nllLoss :: KnownNat n => ParaLens' (Vector n) (Vector n) Double
nllLoss = lens view update
  where
  view (y', y) = -(log $ Monolish.sum (y' * y))
  update (y', y) z =
    let coef = z / Monolish.sum (y' * y)
        error = Monolish.scal coef y'
     in (error, error)

learningRate :: ParaLens' () Double ()
learningRate = lens (const ()) update
  where
  update ((), loss) () = ((), 0.01 * loss)

modelXOR :: ParaLens' _ (Vector 2) (Vector 2)
modelXOR = bias % linear @4 % relu % bias % linear % softmax

modelMNIST :: ParaLens' _ (Vector 784) (Vector 10)
modelMNIST = bias % linear @128 % relu % bias % linear % softmax

class Updatable a where
  update :: a -> a -> a

instance Updatable () where
  update () () = ()

instance KnownNat n => Updatable (Vector n) where
  update !v !w = v + w

instance Updatable (Matrix m n) where
  update !a !b = a `Monolish.matadd` b

instance (Updatable a, Updatable b) => Updatable (a, b) where
  update (!a, !b) (!a', !b') = (update a a', update b b')

updateParam :: (Updatable p, KnownNat n) => ParaLens' p a (Vector n) -> a -> Vector n -> p -> p
updateParam model a b p =
  let l = model % nllLoss % learningRate
      (((p', _), ()), _) = (((p, b), ()), a) & l .~ ()
   in update p p'

train :: (Updatable p, KnownNat n) => ParaLens' p a (Vector n) -> [(a, Vector n)] -> p -> p
train model dataset init = foldl (\p (a, b) -> updateParam model a b p) init dataset

trainV :: (Updatable p, KnownNat n) => ParaLens' p a (Vector n) -> V.Vector (a, Vector n) -> p -> p
trainV model dataset init = V.foldl' (\p (a, b) -> updateParam model a b p) init dataset

accuracyV :: (Updatable p, KnownNat n) => ParaLens' p a (Vector n) -> V.Vector (a, Vector n) -> p -> Double
accuracyV model dataset p = V.foldl' (\count (a, b) -> if ((p, b), a) ^. (model % nllLoss) <= (-log 0.5) then 1.0 else 0.0) 0.0 dataset / fromIntegral (V.length dataset)

mnist :: IO ()
mnist = do
  hSetBuffering stdout NoBuffering

  putStrLn "Loading data"
  (trainXY', testXY') <- MNIST.load
  let trainXY = V.take 3000 trainXY'
      testXY  = V.take 1000 testXY'
  putStrLn $ "train data: " ++ show (V.length trainXY)
  putStrLn $ "test data: " ++ show (V.length testXY)

  putStrLn "Start training"
  bias1   <- Monolish.randomVector @784      (-0.1) 0.1
  weight1 <- Monolish.randomMatrix @128 @784 (-0.1) 0.1
  bias2   <- Monolish.randomVector @128      (-0.1) 0.1
  weight2 <- Monolish.randomMatrix @10  @128 (-0.1) 0.1

  let init = (((((bias1, weight1), ()), bias2), weight2), ())

  flip fix (30, init) $ \loop (n, params) -> do
    if n == 0
      then pure ()
      else do
        let epoch = 30 - n + 1
        putStr $ "epoch: " ++ show epoch
        let trained = trainV modelMNIST trainXY params
            accuracy = accuracyV modelMNIST testXY trained
        putStrLn $ ", accuarcy: " ++ take 4 (show (100 * accuracy)) ++ "%"
        print accuracy
        loop (n-1, trained)


main :: IO ()
main = do
  let dataset = take 10000 $ cycle
                  [ (Monolish.fromListV [0, 0], Monolish.fromListV [1, 0])
                  , (Monolish.fromListV [0, 1], Monolish.fromListV [0, 1])
                  , (Monolish.fromListV [1, 0], Monolish.fromListV [0, 1])
                  , (Monolish.fromListV [1, 1], Monolish.fromListV [1, 0])
                  ]

  bias1   <- Monolish.randomVector @2    (-1) 1
  weight1 <- Monolish.randomMatrix @4 @2 (-1) 1
  bias2   <- Monolish.randomVector @4    (-1) 1
  weight2 <- Monolish.randomMatrix @2 @4 (-1) 1

  let init = (((((bias1, weight1), ()), bias2), weight2), ())
      trained = train modelXOR dataset init
      format d = show $ fromIntegral (floor $ d * 1000) / 1000

  putStrLn "~~~ Result ~~~"
  putStr "(0, 0): "
  putStrLn . format $ Monolish.dot (Monolish.fromListV [0, 1]) $ (trained, Monolish.fromListV [0, 0]) ^. modelXOR
  putStr "(0, 1): "
  putStrLn . format $ Monolish.dot (Monolish.fromListV [0, 1]) $ (trained, Monolish.fromListV [0, 1]) ^. modelXOR
  putStr "(1, 0): "
  putStrLn . format $ Monolish.dot (Monolish.fromListV [0, 1]) $ (trained, Monolish.fromListV [1, 0]) ^. modelXOR
  putStr "(1, 1): "
  putStrLn . format $ Monolish.dot (Monolish.fromListV [0, 1]) $ (trained, Monolish.fromListV [1, 1]) ^. modelXOR
  putStrLn "=============="
