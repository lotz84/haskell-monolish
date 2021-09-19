{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications #-}

module Example.MNIST where

import Data.Function (fix)
import GHC.Exts (IsList(..))
import System.IO

import qualified Data.Vector as V

import DNN
import qualified MNIST as MNIST
import qualified Monolish as Monolish
import Monolish.Types

model :: ParaLens' _ (Vector 784) (Vector 10)
model = bias % linear @128 % relu % bias % linear

example :: IO ()
example = do
  hSetBuffering stdout NoBuffering

  putStrLn "Loading data"
  (trainXY, testXY) <- MNIST.load
  putStrLn $ "train data: " ++ show (V.length trainXY)
  putStrLn $ "test data: "  ++ show (V.length testXY)

  putStrLn "Start training"
  bias1   <- Monolish.randomVector @784      (-0.0837) 0.0837
  weight1 <- Monolish.randomMatrix @128 @784 (-0.0837) 0.0837
  bias2   <- Monolish.randomVector @128      (-0.2085) 0.2085
  weight2 <- Monolish.randomMatrix @10  @128 (-0.2085) 0.2085

  let init = ((((bias1, weight1), ()), bias2), weight2)
      nEpoch = 20

  trained <- flip fix (nEpoch, init) $ \loop (n, params) -> do
    if n == 0
      then pure params
      else do
        let epoch = nEpoch - n + 1
        putStr $ "epoch: " ++ show epoch
        let trained = trainV model trainXY params
            accuracy = accuracyV model testXY trained
        putStrLn $ ", accuarcy: " ++ take 4 (show (100 * accuracy)) ++ "%"
        loop (n-1, trained)

  putStrLn "Show result"
  V.forM_ (V.take 10 testXY) $ \(x, _) -> do
    MNIST.display x
    let prediction = eval (model % softmax) (trained, ()) x
        (score, estimate) = maximum $ zip (toList prediction) [0..9]
    putStrLn $ "Estimate: " ++ show estimate
    putStrLn $ "Score: " ++ show score