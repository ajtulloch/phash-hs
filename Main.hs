{-# LANGUAGE RecordWildCards #-}
module Main(main) where

import           PHash

import           Control.Applicative
import           Control.Monad
import qualified Data.ByteString     as BS
import           Data.Maybe
import           Data.Serialize
import           Options.Applicative
import           System.Log.Logger
import           Text.Printf

data Command = Build BuildOptions | Query QueryOptions
data BuildOptions = BuildOptions {baseDir :: FilePath, serializeTo :: FilePath}
data QueryOptions = QueryOptions {image :: FilePath, deserializeFrom :: FilePath, threshold :: Distance}

loggerName :: String
loggerName = "PHash"

buildOpts :: Parser BuildOptions
buildOpts = BuildOptions
       <$> strOption
               (long "base-directory"
                <> help "Directory to search for images"
                <> metavar "DIR")
       <*> strOption
               (long "serialized"
                <> help "Output serialized"
                <> metavar "SERIALIZED")

queryOpts :: Parser QueryOptions
queryOpts = QueryOptions
       <$> strOption
               (long "image"
                <> help "images to find"
                <> metavar "DIR")
       <*> strOption
               (long "serialized"
                <> help "Input serialized tree"
                <> metavar "SERIALIZED")
       <*> (Distance <$> option auto
               (long "threshold"
                <> help "hamming distance threshold"
                <> metavar "THRESHOLD"))

opts :: Parser Command
opts = subparser
       (command "build" (info (Build <$> buildOpts) (progDesc "Build a PHash tree")) <>
        command "query" (info (Query <$> queryOpts) (progDesc "Query a PHash tree with an image")))


run :: Command -> IO ()
run (Build (BuildOptions{..})) = do
  tree <- buildTree baseDir
  infoM loggerName $ printf "Built tree with %d entries" (numEntries tree)
  debugM loggerName (show tree)
  BS.writeFile serializeTo (encode tree)
run (Query (QueryOptions{..})) = do
  serialized <- BS.readFile deserializeFrom
  query <- fromJust <$> imageHash image
  let Right tree = decode serialized
  infoM loggerName $ printf "Loaded tree with %d entries" (numEntries tree)
  debugM loggerName (show tree)
  forM_ (topMatches tree query threshold) (\(p, Distance d) -> putStr (printf "%d: %s\n" d p :: String))

main :: IO ()
main = do
  updateGlobalLogger loggerName (setLevel INFO)
  args <- execParser (info (helper <*> opts) idm)
  run args
