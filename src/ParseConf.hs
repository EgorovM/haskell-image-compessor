module ParseConf where

import Conf
import Text.Read (readMaybe)

updateConf :: Conf -> String -> String -> Conf
updateConf conf key value =
    case key of
        "-n" -> conf { n = readMaybe value :: Maybe Int }
        "-l" -> conf { l = readMaybe value :: Maybe Double }
        "-f" -> conf { f = Just value }

parseArgs :: Conf -> [String] -> Maybe Conf
parseArgs conf [] = Just conf
parseArgs conf (key:value:args) = parseArgs (updateConf conf key value) args
parseArgs _ _ = Nothing

getOpts :: Conf -> [String] -> Maybe Conf
getOpts conf args = parseArgs conf args
