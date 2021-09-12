{-# LANGUAGE DataKinds #-}


module MNIST where

import Data.Bits
import Data.Word

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.Vector as V

import qualified Monolish as Monolish
import Monolish.Types


fromOctets :: [Word8] -> Word32
fromOctets = foldl accum 0
  where
    accum a o = (a `shiftL` 8) .|. fromIntegral o

loadLabels :: FilePath -> IO (V.Vector Word8)
loadLabels path = do
  contents <- BS.drop 4 <$> BS.readFile path
  let (num', labels) = BS.splitAt 4 contents
      num = fromOctets (BS.unpack num')
  pure $ V.generate (fromIntegral num) (BS.index labels)

loadImages :: FilePath -> IO (V.Vector ByteString)
loadImages path = do
  contents <- BS.drop 4 <$> BS.readFile path
  let (info, images) = BS.splitAt 12 contents
      (num', (height', width')) = BS.splitAt 4 <$> BS.splitAt 4 info
      num    = fromOctets (BS.unpack num')
      height = fromOctets (BS.unpack height')
      width  = fromOctets (BS.unpack width')
  pure $ V.unfoldrN (fromIntegral num) (\bs -> Just $ BS.splitAt (fromIntegral $ height * width) bs) images

load' :: IO (V.Vector ByteString, V.Vector Word8, V.Vector ByteString, V.Vector Word8)
load' = do
  trainX <- loadImages "data/train-images-idx3-ubyte"
  trainY <- loadLabels "data/train-labels-idx1-ubyte"
  testX <- loadImages "data/t10k-images-idx3-ubyte"
  testY <- loadLabels "data/t10k-labels-idx1-ubyte"
  pure (trainX, trainY, testX, testY)

load :: IO (V.Vector (Vector 784, Vector 10), V.Vector (Vector 784, Vector 10))
load = do
  (trainX, trainY, testX, testY) <- load'
  let convert image label = ( Monolish.fromListV $ (\w -> fromIntegral w / 128 - 1) <$> BS.unpack image
                            , Monolish.fromListV $ (\i -> if i == fromIntegral label then 1 else 0) <$> [0..9]
                            )
      trainXY = V.zipWith convert trainX trainY
      testXY = V.zipWith convert testX testY
  pure (trainXY, testXY)