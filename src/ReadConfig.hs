module ReadConfig (
	parseConfigFile
) where

import ReadConfigPappy

main :: IO ()
main = interact $ show . parseConfigFile
