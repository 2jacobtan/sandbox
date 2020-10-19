{-# LANGUAGE ScopedTypeVariables #-}

-- see hgeometry/hgeometry-examples/src/Demo/GeneratePlanarSubdivisions.hs

module LibVoronoi where

import Control.Applicative (liftA3)
import Control.Monad (replicateM, liftM)
import Control.Monad.Trans.State
import System.Random
import GHC.Word
import Data.List (unfoldr)
import Codec.Picture as CP
import Data.Array as DA

import           Algorithms.Geometry.DelaunayTriangulation.DivideAndConquer
import           Algorithms.Geometry.DelaunayTriangulation.Types
import           Control.Lens
import           Data.Data
import           Data.Ext
import           Data.Geometry
import           Data.Geometry.Ipe
import qualified Data.List.NonEmpty as NonEmpty
import           Data.PlaneGraph
import           Data.PlanarGraph as PG
import           Data.Semigroup
import           Data.Yaml.Util
import           Options.Applicative
-- import           Test.QuickCheck
-- import           Test.QuickCheck.HGeometryInstances ()


someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- https://twitter.com/joshu/status/1317297227019157504
-- i have a large numpy array (from an image, millions of values)
-- and a thousands of points.
-- i want to calculate the sum and total of each point's nearest pixels.
-- is there a way to do this actually quickly?
-- if i just iterate over the array it takes forever.

-- we work with a "photo" -- some megapixel JPG off a camera or whatever
-- and a "voronoi" set of points.
-- deriving the voronoi from the photo is a separate exercise.

-- VORONOI

-- to have something to work with,
-- we randomly generate an image of sparse points, represented as an one-dimensional array of bools, of length x*y

mkRandArray :: Int -> Int -> Int -> Array Int Bool
mkRandArray imgX imgY cutoff = listArray (0, imgX * imgY) $
                               ((< cutoff) . fromIntegral)
                               <$> (randoms (mkStdGen 137) :: [Word16])

-- so we can eyeball it, our intermediate representation is a JuicyPixels 8-bit RGB bitmap suitable for outputting to PNG

mkImage :: Int -> Int -> Int -> Array Int Bool -> CP.Image PixelRGB8
mkImage imgX imgY cutoff myrands = do
  let pixelRenderer x y = 
        if (myrands DA.! (y * imgX + x))
        then PixelRGB8 255 255 255
        else PixelRGB8 0 0 0
  CP.generateImage pixelRenderer imgX imgY

-- some stats on the voronoi
arrayStats :: Array Int Bool -> String
arrayStats ary = unlines [ "* stats on the voronoi array:"
                         , unwords [ show lit, "/", show size
                                   , "pixels are lit" ] ]
  where lit  = length (filter id $ elems ary)
        size = snd (bounds ary) - fst (bounds ary)

writePng :: PngSavable a => String -> CP.Image a -> IO ()
writePng = CP.writePng

-- given an image file, we can now compute the Delaunay triangulation
-- and from the Delaunay triangulation we can compute the Voronoi diagram



-- see also
-- https://github.com/noinia/hgeometry/tree/master/hgeometry-examples/src/Demo/GeneratePlanarSubdivisions.hs


-- once we have the Voronoi diagram we can compute the 

imageValues = ()


-- algorithm:

-- given the Voronoi diagram, color the map uniquely, so that each region's innards are painted that region's colour.
-- the flood-fill algorithm is O(n) in the number of pixels
-- https://en.wikipedia.org/wiki/Flood_fill#:~:text=Flood%20fill%2C%20also%20called%20seed,in%20a%20multi%2Ddimensional%20array.

-- then, for each point in the input image of n pixels, look up the color of the painted pixel in the Voronoi map.
-- this lookup should be O(n * 1)

-- any point which falls on an edge may be more accurately resolved using authoritative evaluation against the voronoi plane


