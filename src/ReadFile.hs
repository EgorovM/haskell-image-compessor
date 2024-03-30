module ReadFile where

import Control.Exception (catch, IOException)
import ImagePoint

type Point = (Int, Int)
type Color = (Short, Short, Short)
type Pixel = (Point, Color)
type Short = Int

processFile :: FilePath -> IO[Pixel]
processFile filePath = do
    content <- readFile filePath
    let linesOfFile = lines content
    return $ map parseLine linesOfFile

parseLine :: String -> Pixel
parseLine line =
    let (pointStr, colorStr) = break (== ' ') line
        point = read pointStr :: Point
        color = read (drop 1 colorStr) :: Color
    in (point, color)

handleIOError :: IOException -> IO ()
handleIOError e = print e
