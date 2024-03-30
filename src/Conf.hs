module Conf where

data Conf = Conf {
    n :: Maybe Int,
    l :: Maybe Double,
    f :: Maybe String
} deriving (Show)

defaultConf :: Conf
defaultConf = Conf {
    n = Just 2,
    l = Just 0.8, 
    f = Nothing
}