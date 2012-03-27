import Data.List
import Data.Char
import qualified Data.Map as Map
import qualified Data.Set as Set

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub


{-- Char --}
encode :: Int -> String -> String
encode shift msg =
	let ords	= map ord msg
	    shifted	= map (+ shift) ords
	in  map chr shifted

decode :: Int -> String -> String
decode shift msg = encode (negate shift) msg


{-- Map --}
findKey :: (Eq k) => k -> [(k, v)] -> Maybe v
findKey key [] = Nothing
findKey key ((k,v):xs) = if key == k
				then Just v
				else findKey key xs

findKey' :: (Eq k) => k -> [(k, v)] -> Maybe v
findKey' key = foldr (\ (k, v) acc -> if k == key then Just v else acc) Nothing

fromList' :: (Ord k) => [(k, v)] -> Map.Map k v
fromList' = foldr (\ (k, v) acc -> Map.insert k v acc) Map.empty
