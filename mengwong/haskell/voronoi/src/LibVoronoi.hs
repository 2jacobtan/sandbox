module LibVoronoi where

import Control.Applicative (liftA3)
import Control.Monad (replicateM, liftM)
import Control.Monad.Trans.State
import System.Random
import GHC.Word
import Data.List (unfoldr)
import Codec.Picture

import Data.Array

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- https://twitter.com/joshu/status/1317297227019157504
-- i have a large numpy array (from an image, millions of values)
-- and a thousands of points.
-- i want to calculate the sum and total of each point's nearest pixels.
-- is there a way to do this actually quickly?
-- if i just iterate over the array it takes forever.

-- we randomly generate an image of sparse points
-- by first setting up a monadic random number generator

-- imageCreator :: String -> IO ()
-- imageCreator path = writePng path $ generateImage pixelRenderer 250 300
--    where pixelRenderer x y = PixelRGB8 (fromIntegral x) (fromIntegral y) 128

imageCreator :: Int -> Int -> Int -> String -> IO ()
imageCreator imgX imgY cutoff path = do
  let myrands = listArray (0, imgX * imgY) (randoms (mkStdGen 137) :: [Word16])
      pixelRenderer x y = 
        if (fromIntegral $ myrands ! (x*y)) < cutoff
        then PixelRGB8 0 0 0
        else PixelRGB8 255 255 255
  writePng path $ generateImage pixelRenderer imgX imgY

imageValues = ()



