{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications #-}

module Example.XOR where

import qualified Data.Vector as V

import DNN
import qualified Monolish as Monolish
import Monolish.Types

model :: ParaLens' _ (Vector 2) (Vector 2)
model = bias % linear @4 % relu % bias % linear

dataset :: [(Vector 2, Vector 2)]
dataset = take 100000 $ cycle
  [ ([0, 0], [1, 0])
  , ([0, 1], [0, 1])
  , ([1, 0], [0, 1])
  , ([1, 1], [1, 0])
  ]

example :: IO ()
example = do
  bias1   <- Monolish.randomVector @2    (-1.224) 1.224
  weight1 <- Monolish.randomMatrix @4 @2 (-1.224) 1.224
  bias2   <- Monolish.randomVector @4    (-1.224) 1.224
  weight2 <- Monolish.randomMatrix @2 @4 (-1.224) 1.224

  let init = ((((bias1, weight1), ()), bias2), weight2)
      trained = train model dataset init

  putStrLn "~~~ Result ~~~"
  putStr "(0, 0): "
  putStrLn . format . getOneProb $ eval (model % softmax) (trained, ()) [0, 0]
  putStr "(0, 1): "
  putStrLn . format . getOneProb $ eval (model % softmax) (trained, ()) [0, 1]
  putStr "(1, 0): "
  putStrLn . format . getOneProb $ eval (model % softmax) (trained, ()) [1, 0]
  putStr "(1, 1): "
  putStrLn . format . getOneProb $ eval (model % softmax) (trained, ()) [1, 1]
  putStrLn "=============="
  where
  getOneProb = Monolish.dot [0, 1]
  format d = show $ fromIntegral (floor $ d * 1000) / 1000