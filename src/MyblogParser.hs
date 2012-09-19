module MyblogParser (
	parseInputText
) where

import Control.Applicative
import MyblogParserPappy

main = print =<< parseInputText <$> readFile "sample.txt"
