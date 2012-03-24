lucky :: (Integral a) => a -> String
lucky 7 = "Lucky number seven!"
lucky x = "Sorry, you are not lucky!"

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors a b = (fst a + fst b, snd a + snd b)

addVectors' :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors' (x1, x2) (y1, y2) = (x1 + y1, x2 + y2)

first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, x, _) = x

third :: (a, b, c) -> c
third (_, _, x) = x

head' :: [a] -> a
head' [] = error "List cannot be empty."
head' (x:_) = x

length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

capital :: String -> String
capital [] = "Whoops, were is your string?"
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x] ++ "."

{-- Guards --}
bmiTell :: (RealFloat a) => a -> String
bmiTell bmi
	| bmi <= 18.5 = "You are underweight, skinny bitch!"
	| bmi <= 25.0 = "Boring, just average normal he'"
	| bmi <= 30.0 = "Fat! Better lose some weight!"
	| otherwise   = "You're a whale, congrats!"

bmiTell' :: (RealFloat a) => a -> a -> String
bmiTell' weight height
        | bmi <= skinny = "You are underweight, skinny bitch!"
        | bmi <= normal = "Boring, just average normal he'"
        | bmi <= fat = "Fat! Better lose some weight!"
        | otherwise   = "You're a whale, congrats!"
	where bmi = weight / height ^ 2
	      skinny = 18.5
	      normal = 25.0
	      fat = 30.0

initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
	where (f:_) = firstname
	      (l:_) = lastname

calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi w h | (w, h) <- xs]
	where bmi weight height = weight / height ^ 2

{-- let in  --}
cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
	let sideArea = 2 * pi * r * h
	    topArea = pi * r ^ 2
	in  sideArea + 2 * topArea

calcBmis' :: (RealFloat a) => [(a, a)] -> [a]
calcBmis' xs = [bmi | (w, h)  <- xs, let bmi = w / h * 2]
