module Processing (
	Diary,
	addUUID,
	lookD
) where

import Data.UUID.V4
import Data.Maybe

type Diary = ([(String, String)], [String])

addUUID :: Diary -> IO (Diary, Maybe String)
addUUID (tags, cnt) = do
	let	uu = lookup "uuid" tags
	case uu of
		Just _ -> return ((tags, cnt), uu)
		_ -> do	u <- nextRandom
			return ((("uuid", show u) : tags, cnt), uu)

lookD :: String -> Diary -> String
lookD t = look t . fst

look :: (Show a, Eq a) => a -> [(a, b)] -> b
-- look = (.) fromJust . lookup
look x ps
	| Just y <- lookup x ps = y
	| otherwise = error $ "not key " ++ show x
