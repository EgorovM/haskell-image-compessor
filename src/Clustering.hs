module Clustering where

import qualified Data.Map.Strict as Map
import Data.List (minimumBy, groupBy, elemIndex)
import Data.Function (on)
import Data.Ord (comparing)

import ImagePoint


distance :: ImagePoint -> ImagePoint -> Double
distance (ImagePoint r1 g1 b1 _ _) (ImagePoint r2 g2 b2 _ _) =
    sqrt $ fromIntegral $ (r2 - r1)^2 + (g2 - g1)^2 + (b2 - b1)^2


centroid :: [ImagePoint] -> ImagePoint
centroid ps = makeImagePoint (sum (map r ps) `div` length ps) (sum (map g ps) `div` length ps) (sum (map b ps) `div` length ps)


assignToClusters :: [ImagePoint] -> [ImagePoint] -> [[ImagePoint]]
assignToClusters centroids imagePoints = Map.elems $ foldr insertPoint Map.empty imagePoints
  where
    nearestCentroid p = minimumBy (comparing (distance p)) centroids
    insertPoint p m = let c = nearestCentroid p in Map.insertWith (++) (centroidIndex c) [p] m
    centroidIndex c = maybe 0 id $ c `elemIndex` centroids


converged :: Double -> [ImagePoint] -> [ImagePoint] -> Bool
converged l oldCentroids newCentroids = all (< l) $ zipWith distance oldCentroids newCentroids


kMeans :: Double -> [ImagePoint] -> [ImagePoint] -> [[ImagePoint]]
kMeans l centroids imagePoints
    | converged l centroids newCentroids = clusters
    | otherwise = kMeans l newCentroids imagePoints
  where clusters = assignToClusters centroids imagePoints
        newCentroids = map centroid clusters
