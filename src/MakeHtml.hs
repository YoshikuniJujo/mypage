module MakeHtml (
	makeHtml
) where

import Data.Maybe
import Data.Char

makeHtml :: [([(String, String)], [String])] -> IO String
makeHtml dat = do
	footer <- readFile "footer.txt"
	return $ xmlheader ++ doctype ++ htmlOpen ++ header ++
		body (unlines (map makeTC dat) ++ footer) ++ htmlClose

makeLink1FromString :: String -> String
makeLink1FromString src = makeLink1 (takeWhile (/= ',') src) $
	dropWhile isSpace $ tail $ dropWhile (/= ',') src

makeLink1 :: String -> String -> String
makeLink1 title address =
	"<p><a href=\"" ++ address ++ "\">" ++ title ++ "</a></p>"

makeLinks :: [String] -> String
makeLinks = unlines . map makeLink1FromString

makeTC :: ([(String, String)], [String]) -> String
makeTC (tags, cnt) = makeTitle tags ++
	makeLinks (map snd $ filter ((== "link") . fst) tags) ++
--	makeLink1FromString "gentoo, http://gentoo.org" ++
	"<p>" ++ makeContent cnt ++ "</p>\n"

makeContent :: [String] -> String
makeContent =
	unlines . map (++ "<br/>") . dropWhile null . reverse . dropWhile null . reverse

makeTitle :: [(String, String)] -> String
makeTitle tags = "<h2>" ++ fromJust (lookup "title" tags) ++ "</h2>\n"

xmlheader = "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>\n"
doctype = "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" " ++
	"\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">\n"
htmlOpen = "<html xmlns=\"http://www.w3.org/1999/xhtml\" lang=\"ja_JP\">"
htmlClose = "</html>\n"

header = "<head><title>iocikun. snukarni</title></head>\n"
body cnt = "<body>" ++ cnt ++ "</body>"
