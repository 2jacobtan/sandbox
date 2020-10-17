module Main where

import LibVoronoi

import Options.Applicative

data Config = Config
  { outfile :: String
  , infile :: String
  , x      :: Int
  , y      :: Int
  , cutoff :: Int }

optconfig :: Parser Config
optconfig = Config
      <$> strOption
          ( long "out"
         <> metavar "FILE"
         <> help "output filename" )
      <*> strOption
          ( long "in"
         <> metavar "FILE"
         <> value "-"
         <> help "input filename" )
      <*> option auto
          ( short 'x'
         <> help "x dimension, pixels"
         <> showDefault
         <> value 1024
         <> metavar "INT" )
      <*> option auto
          ( short 'y'
         <> help "y dimension, pixels"
         <> showDefault
         <> value 768
         <> metavar "INT" )
      <*> option auto
          ( long "cutoff"
         <> help "randomness cutoff for voronoi plane density, out of 2^16"
         <> showDefault
         <> value 10
         <> metavar "INT" )


main :: IO ()
main = do
  opts <- execParser $ info (optconfig <**> helper)
          ( fullDesc
            <> progDesc "compute pixel stats by voronoi cell"
            <> header "voronoi" )
  imageCreator (x opts) (y opts) (cutoff opts) (outfile opts)
  where


