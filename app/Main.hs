module Main where

import Data.Foldable (for_)
import Data.List (lookup)
import System.Environment (getArgs)

import qualified Example.XOR as XOR
import qualified Example.MNIST as MNIST

examples :: [(String, IO ())]
examples = [ ("xor",   XOR.example)
           , ("mnist", MNIST.example)
           ]

main :: IO ()
main = do
  args <- getArgs
  case args of
    []       -> notfound
    (arg: _) -> maybe notfound id (lookup arg examples)
  where
  notfound = do
    putStrLn "Please give an argument beloniging the below."
    for_ examples $ \(name, _) ->
      putStrLn $ "- " ++ name