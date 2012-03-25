multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z

multTwoWithNine :: (Num a) => a -> a -> a
multTwoWithNine = multThree 9

compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred x = compare 100 x

compareWithHundred' :: (Num a, Ord a) => a -> Ordering
compareWithHundred' = compare 100

divideByThen :: (Floating a) => a -> a
divideByThen = (/10)

isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A'..'Z'])

{-- Taking functions as parameters --}
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f x y = f y x

flip'' :: (a -> b -> c) -> (b -> a -> c)
flip'' f = g
	where g x y = f y x

map' :: (a -> b) -> [a] -> [b]
map' f xs = [f x | x <- xs]

map'' :: (a -> b) -> [a] -> [b]
map'' _ [] = []
map'' f (x:xs) = f x : map'' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs)
	| f x = x : filter' f xs
	| otherwise = filter' f xs

largestDivisibleHundredThousand :: (Integral a) => a
largestDivisibleHundredThousand = head (filter p [100000,99999..])
	where p x = x `mod` 3829 == 0

chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain x
	| even x = x : chain (x `div` 2)
	| odd x = x : chain (x * 3 + 1)

numLongChains :: Int
numLongChains = length (filter isLongChain (map chain [1..100]))
	where isLongChain xs
		| length xs > 15 = True
		| otherwise 	 = False

numLongChains' :: Int
numLongChains' = length (filter isLongChain (map chain [1..100]))
	where isLongChain xs = length xs > 15


{--  Lambdas --}
numLongChains'' :: Int
numLongChains'' = length (filter (\xs -> length xs > 15) (map chain [1..100]))

flip''' :: (a -> b -> c) -> (b -> a -> c)
flip''' f = \a b -> f b a


{-- Folds --}
sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs

-- Same fold as above, but taking advantage of currying
sum'' :: (Num a) => [a] -> a
sum'' = foldl (+) 0

elem' :: (Eq a) => a -> [a] -> Bool
elem' y ys = foldl (\acc x -> if x == y then True else acc) False ys

-- use right folds for building new lists
-- since : is less expensive than ++
map''' :: (a -> b) -> [a] -> [b]
map''' f ys = foldr (\x acc -> f x : acc) [] ys

-- do foldl1 to automatically have the correct type of the values of the list
maximumf :: (Ord a) => [a] -> a
maximumf xs = foldl1 (\acc x -> if acc < x then x else acc) xs

maximumf' :: (Ord a) => [a] -> a
maximumf' = foldl1 (\acc x -> if acc < x then x else acc)

reversef :: [a] -> [a]
reversef = foldl (\acc x -> x : acc) []

productf :: (Num a) => [a] -> a
productf = foldl1 (*)

filterf :: (a -> Bool) -> [a] -> [a]
filterf f ys = foldr (\ x acc -> if f x then x : acc else acc) [] ys

filterf' :: (a -> Bool) -> [a] -> [a]
filterf' f = foldr (\ x acc -> if f x then x : acc else acc) []

headf :: [a] -> a
headf = foldl1 (\ acc x -> acc)

headf' :: [a] -> a
headf' = foldr1 (\ x _ -> x)

lastf :: [a] -> a
lastf = foldr1 (\ _ acc -> acc)

lastf' :: [a] -> a
lastf' = foldl1 (\ _ x -> x)

-- Next: function application
