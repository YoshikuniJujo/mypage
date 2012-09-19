import XMLFile(xmlfileToData, makeXmlfile)
import MakeHtml(makeHtml)
import MyblogParser(parseInputText)
import Processing(addUUID, Diary, lookD)
import System.Environment(getProgName, getArgs)
import Data.List(isPrefixOf)
import Data.Maybe
import System.IO(hFlush, stdout)
import System.Cmd
import ReadConfig
import Control.Applicative
import Network.Yjftp

main = do
	bn : args <- getArgs
	config <- parseConfigFile <$> readFile "config.txt"
	let	editor = fromMaybe (error "no editor") $ lookup "editor" config
		server = fromMaybe (error "no server") $ lookup "server" config
		directory = fromMaybe (error "no diectory") $ lookup "directory" config
		account = fromMaybe (error "no account") $ lookup "account" config
	case args of
		("list" : largs) -> listDiary bn largs
		("remove" : rargs) -> removeDiary bn rargs
		("edit" : rargs) -> do
			getDiary bn rargs
			rawSystem editor ["diary.txt"]
			putDiary bn "diary.txt"
			return ()
		("new" : nargs) -> do
			newDiary bn nargs
			rawSystem editor ["diary.txt"]
			putDiary bn "diary.txt"
			return ()
		("move" : margs) -> moveDiary bn margs
		("rewrite" : rargs) -> rewriteUuidlenIO bn
		("upload" : _) -> yjftp (Just Put) (Just $ bn ++ ".html")
			(Just server) (Just account) (Just directory) Nothing
		[fn] -> putDiary bn fn

newDiary :: String -> [String] -> IO ()
newDiary _ _ = do
	putStr "title: "
	hFlush stdout
	title <- getLine
	writeFile "diary.txt" $ "title: " ++ title ++ "\n\n.\n"

putDiary :: String -> FilePath -> IO ()
putDiary blogName fn = do
	txt <- readFile fn
	olds <- xmlfileToData $ blogName ++ ".xml"
	let	new = parseInputText txt
	(new', uuid) <- addUUID new
	let	dat = maybe (addNew new' olds) (\u -> editDiary u new' olds) uuid
	makeXmlfile (blogName ++".xml") dat
	writeFile (blogName ++ ".html") =<< makeHtml dat

addNew :: Diary -> [Diary] -> [Diary]
addNew d@(tags, cnt) ds = let
	len = getLen (lookD "uuid" d) (map (lookD "uuid") ds) + 1
	nd = (("uuidlen", show len) : tags, cnt) in
	nd : ds

editDiary :: String -> Diary -> [Diary] -> [Diary]
editDiary _ _ [] = error "bad UUID"
editDiary u dn@(tags, cnt) (d : ds)
	| checkUUID u d = (("uuidlen", lookD "uuidlen" d) : tags, cnt) : ds
	| otherwise = d : editDiary u dn ds

listDiary :: String -> [String] -> IO ()
listDiary blogName largs = do
	olds <- xmlfileToData $ blogName ++ ".xml"
	let	u = lookD "uuid"
		t = lookD "title"
		l = read . lookD "uuidlen"
	putStr $ unlines $
		map (\o -> take (l o) (u o) ++ replicate (8 - l o) ' ' ++ t o) olds

removeDiary :: String -> [String] -> IO ()
removeDiary blogName [u] = do
	olds <- xmlfileToData $ blogName ++ ".xml"
	let	new = deleteDiary u olds
	makeXmlfile (blogName ++ ".xml") $ rewriteUuidlen new
	writeFile (blogName ++ ".html") =<< makeHtml new

deleteDiary :: String -> [Diary] -> [Diary]
deleteDiary u = reverse . dd . reverse
	where
	dd (d : ds)
		| checkUUID u d = ds
		| otherwise = d : dd ds

getDiary :: String -> [String] -> IO ()
getDiary blogName [u] = do
	olds <- xmlfileToData $ blogName ++ ".xml"
	let	d = getD u olds
	writeFile "diary.txt" $ makeText d

getD :: String -> [Diary] -> Diary
getD u = gd . reverse
	where
	gd (d@(tags, cnt) : ds)
		| checkUUID u d = (filter ((/= "uuidlen") . fst) tags, cnt)
		| otherwise = gd ds

rewriteUuidlenIO :: String -> IO ()
rewriteUuidlenIO blogName = do
	olds <- xmlfileToData $ blogName ++ ".xml"
	makeXmlfile (blogName ++ ".xml") $ rewriteUuidlen olds

rewriteUuidlen :: [Diary] -> [Diary]
rewriteUuidlen ds
	= foldr addNew [] $ map deleteUuidlen ds

deleteUuidlen :: Diary -> Diary
deleteUuidlen (tags, cnt) = (filter ((/= "uuidlen") . fst) tags, cnt)

moveDiary :: String -> [String] -> IO ()
moveDiary blogName [u, p] = do
	olds <- xmlfileToData $ blogName ++ ".xml"
	let	d = getD u olds
		r = deleteDiary u olds
		new = take (read p) r ++ [d] ++ drop (read p) r
	makeXmlfile (blogName ++ ".xml") $ rewriteUuidlen new
	writeFile (blogName ++ ".html") =<< makeHtml new

makeText :: Diary -> String
makeText (tags, cnt) =
	unlines (map (\(t, v) -> t ++ ": " ++ v) tags) ++ "\n" ++ unlines cnt ++
	"."

checkUUID :: String -> Diary -> Bool
checkUUID s d = s `isPrefixOf` lookD "uuid" d

getLen :: String -> [String] -> Int
getLen _ [] = 0
getLen s1 (s2 : ss) = sameLen s1 s2 `max` getLen s1 ss

sameLen :: String -> String -> Int
sameLen [] _ = 0
sameLen _ [] = 0
sameLen (x : xs) (y: ys)
	| x == y = 1 + sameLen xs ys
	| otherwise = 0
