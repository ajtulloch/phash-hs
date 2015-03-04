{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ForeignFunctionInterface   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections              #-}
module PHash(PHash, PHashTree(..), numEntries, buildTree, imageHash, topMatches, Distance(..)) where

import           Control.Applicative
import           Control.Monad
import           Data.Function
import           Data.List
import           Data.Maybe
import           Data.Serialize
import qualified Data.Vector           as V
import           Data.Vector.Serialize ()
import           Foreign
import           Foreign.C.String
import           Foreign.C.Types
import           GHC.Generics
import           System.Directory
import           System.FilePath

foreign import ccall unsafe "pHash.h ph_dct_imagehash"
    c_ph_dct_imagehash :: CString -> Ptr CULong -> IO CInt

foreign import ccall unsafe "pHash.h ph_hamming_distance"
    c_ph_hamming_distance :: CULong -> CULong -> CInt

newtype Distance = Distance Int deriving (Num, Eq, Ord, Show)

newtype PHash = PHash Word64 deriving (Show, Eq, Num, Generic)
instance Serialize PHash

data Node = Node PHash FilePath deriving (Show, Generic)
instance Serialize Node

newtype PHashTree = PHashTree (V.Vector Node) deriving (Show, Generic)
instance Serialize PHashTree

hammingDistance :: PHash -> PHash -> Distance
hammingDistance l r = Distance $ fromIntegral x
    where
      CInt x = c_ph_hamming_distance (toCPHash l) (toCPHash r)
      toCPHash (PHash y) = CULong y

imageHash :: FilePath -> IO (Maybe PHash)
imageHash path = withCString path $ \cs ->
                 with (CULong 0) $ \pHPtr -> do
                   h <- c_ph_dct_imagehash cs pHPtr
                   handle pHPtr h
    where
      handle _ (CInt (-1)) = return Nothing
      handle pHPtr _ = Just . fromCPHash <$> peek pHPtr
      fromCPHash (CULong x) = PHash x

numEntries :: PHashTree -> Int
numEntries (PHashTree tree) = V.length tree

topMatches :: PHashTree -> PHash -> Distance -> [(FilePath, Distance)]
topMatches (PHashTree tree) query threshold = go tree
    where
      go = sortBy (compare `on` snd) . V.toList . V.filter ((<= threshold) . snd) . V.map distance
      distance (Node hash path) = (path, hammingDistance hash query)

imagesInDir :: FilePath -> IO [FilePath]
imagesInDir dir = do
  contents <- getDirectoryContents dir
  let filtered = filter (\d -> takeExtension d `elem` [".jpg", ".png"]) contents
  forM filtered (\p -> canonicalizePath (dir </> p))

hashesToTree :: [(Maybe PHash, FilePath)] -> PHashTree
hashesToTree = PHashTree . V.fromList . mapMaybe valid
    where
      valid (Just hash, path) = Just (Node hash path)
      valid (Nothing, _) = Nothing

extractHashes :: FilePath -> IO [(Maybe PHash, FilePath)]
extractHashes baseDir = do
  images <- imagesInDir baseDir
  forM images $ \p -> do
    h <- imageHash p
    return (h, p)


buildTree :: FilePath -> IO PHashTree
buildTree baseDir = hashesToTree <$> extractHashes baseDir
