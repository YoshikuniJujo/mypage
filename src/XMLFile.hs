module XMLFile (
	makeXmlfile,
	xmlfileToData
) where

import Text.XML.HaXml
import Text.XML.HaXml.Pretty
import Data.Maybe
import System.IO

main :: IO ()
main = xmlfileToData "sample.xml" >>= print

myReadFile :: FilePath -> IO String
myReadFile fn = do
	c <- readFile fn
	putStr $ take (length c - length c) "dummy"
	return c

xmlfileToData :: FilePath -> IO [([(String, String)], [String])]
xmlfileToData fn = flip fmap (myReadFile fn) $
	map (\c -> (getTags c, getContent c)) .
	filter isElem . contents . topElem . xmlParse fn

isElem :: Content i -> Bool
isElem (CElem _ _) = True
isElem _ = False

getTags :: Content i -> [(String, String)]
getTags = map fst . (tagged `x` extracted getText) children . head .
	(childrenBy $ tag "tags")

getContent :: Content i -> [String]
getContent c = map fst $ concatMap (extracted getText children) $
	childrenBy (tag "contents") c

maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead (h : _) = Just h

getText :: Content i -> String
-- getText = maybe "" (fromJust . fst) . maybeHead . textlabelled children
getText = concatMap (maybe "bad" id . fst) . textreflabelled children

textreflabelled :: CFilter i -> LabelFilter i (Maybe String)
textreflabelled f = extracted textref f
	where
	textref (CString _ n _) = Just n
	textref (CRef (RefEntity n) _) = Just $ "&" ++ n ++ ";"
	textref _ = Nothing

reflabelled :: CFilter i -> LabelFilter i (Maybe String)
reflabelled f = extracted ref f
	where
	ref (CRef (RefEntity n) _) = Just n
	text _ = Nothing

topElem :: Document i -> Element i
topElem (Document _ _ e _) = e

contents :: Element i -> [Content i]
contents (Elem _ _ cs) = cs

makeXmlfile :: FilePath -> [([(String, String)], [String])] -> IO ()
makeXmlfile fn = writeFile fn . show . document . makeXml

makeXml :: [([(String, String)], [String])] -> Document i
makeXml tcs = Document (Prolog (Just xmlDecl) [] Nothing []) [] (
	Elem (N "diarys") [] (map (flip CElem undefined . uncurry makeDiary) tcs))
	[]

xmlDecl :: XMLDecl
xmlDecl = XMLDecl "1.0" (Just $ EncodingDecl "UTF-8") Nothing

makeDiary :: [(String, String)] -> [String] -> Element i
makeDiary tvs cnt = Elem (N "diary") [] [
	flip CElem undefined $ makeTags tvs,
	flip CElem undefined $ makeLines cnt]

makeTags :: [(String, String)] -> Element i
makeTags tvs = Elem (N "tags") [] $ map (flip CElem undefined . makeTag) tvs

makeTag :: (String, String) -> Element i
makeTag (t, v) = Elem (N t) [] [CString False v undefined]

makeLines :: [String] -> Element i
makeLines ls
	= Elem (N "contents") [] $ map (flip CElem undefined . makeLine) ls

makeLine :: String -> Element i
makeLine l = Elem (N "line") [] [CString False l undefined]

-- makeLineList :: String -> [Elemen
