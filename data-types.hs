data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
	| x == a = Node x left right
	| x < a  = Node a (treeInsert x left) right
	| x > a  = Node a left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
	| x == a = True
	| x < a  = treeElem x left
	| x > a  = treeElem x right

class YesNo a where
	yesno :: a -> Bool

instance YesNo Int where
	yesno 0 = False
	yesno _ = True

instance YesNo [a] where
	yesno [] = False
	yesno _  = True

instance YesNo Bool where
	yesno = id

instance YesNo (Maybe a) where
	yesno Nothing  = False
	yesno (Just _) = True 

instance YesNo (Tree a) where
	yesno EmptyTree = False
	yesno _	        = True

yesnoIf :: (YesNo y) => y -> a -> a  -> a
yesnoIf yesnoVal yesResult noResult = if yesno yesnoVal then yesResult else noResult
