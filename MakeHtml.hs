module MakeHtml (
	makeHtml
) where

import Data.Maybe

makeHtml :: [([(String, String)], [String])] -> String
makeHtml dat =
	xmlheader ++ doctype ++ htmlOpen ++ header ++
	body (unlines (map makeTC dat) ++ counter ++ cont ++ access) ++
	htmlClose

makeTC :: ([(String, String)], [String]) -> String
makeTC (tags, cnt) = makeTitle tags ++ "<p>" ++ makeContent cnt ++ "</p>\n"

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

counter = "<p style=\"text-align:right;\">\n" ++
	"<img src=\"http://hpcounter.nifty.com/cgi-bin/counter.cgi?" ++
	"f=salamander&amp;n=1&amp;d=3\" alt=\"counter\"/>" ++
	"</p>\n"

cont = "<p style=\"text-align:right;\">\n" ++
	"<a href=\"http://validator.w3.org/check?uri=referer\">\n" ++
	"<img src=\"http://www.w3.org/Icons/valid-xhtml10\"\n\t" ++
	"alt=\"Valid XHTML 1.0 Strict\" height=\"31\" width=\"88\"/></a></p>\n"

access = "<p style=\"text-align:right;\">" ++
	"<img src=\"http://hpcgi3.nifty.com/salamander/myblog/powered_by.pl\" " ++
	"alt=\"gentoo\"/>" ++
	"</p>\n"
