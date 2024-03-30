module ImagePoint where

data ImagePoint = ImagePoint {
    r :: Int,
    g :: Int,
    b :: Int,
    x :: Int,
    y :: Int
} deriving (Eq, Show)

makeImagePoint :: Int -> Int -> Int -> ImagePoint
makeImagePoint r g b = ImagePoint r g b 1 1

showImagePoint :: ImagePoint -> String
showImagePoint (ImagePoint r g b x y) = "(" ++ show x ++ "," ++ show y ++ ")" ++ " " ++ "(" ++ show r ++ "," ++ show g ++ "," ++ show b ++ ")"

showImageColor :: ImagePoint -> String
showImageColor (ImagePoint r g b _ _) = "(" ++ show r ++ "," ++ show g ++ "," ++ show b ++ ")"