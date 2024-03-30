module Main (main) where

import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map
import System.Environment (getArgs)
import ParseConf (getOpts)

import System.Random

import ReadFile (processFile)
import Clustering (kMeans, centroid)
import Conf (defaultConf, n, l, f)

import ImagePoint


printClusters :: [[ImagePoint]] -> IO ()
printClusters clusters = mapM_ printCluster $ zip [0..] clusters
  where
    printCluster :: (Int, [ImagePoint]) -> IO ()
    printCluster (i, cluster) = do
        let centerPoint = centroid cluster
        putStrLn "--"
        putStrLn $ showImageColor centerPoint
        putStrLn "-"
        mapM_ printPoint cluster
        where
            centroid ps = let (rs, gs, bs) = unzip3 [(r p, g p, b p) | p <- ps]
                        in makeImagePoint (mean rs) (mean gs) (mean bs)
            mean xs = sum xs `div` length xs
            printPoint point = putStrLn $ showImagePoint point


main :: IO ()
main = do
    args <- getArgs

    putStrLn $ intercalate " " args

    let maybeConf = getOpts defaultConf args

    case maybeConf of
        Just conf -> do
            let defaultN = 5 -- Example default value for n
            let defaultL = 0.01 -- Example default value for l
            let defaultF = "in"
    
            let nValue = fromMaybe defaultN (n conf)
            let lValue = fromMaybe defaultL (l conf)
            let fValue = fromMaybe defaultF (f conf)

            pixels <- processFile fValue
            
            let imagePoints = [ImagePoint r g b x y | ((x, y), (r, g, b)) <- pixels]

            gen <- getStdGen
            let rs = randomRs (0, 255) gen
                gs = randomRs (0, 255) gen
                bs = randomRs (0, 255) gen
                initialCentroids = take nValue $ zipWith3 makeImagePoint rs gs bs

            let clusters = kMeans lValue initialCentroids imagePoints

            printClusters clusters
    
        Nothing -> putStrLn "Error parsing configuration"
