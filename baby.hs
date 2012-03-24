doubleMe x =  x + x

doubleUs x y = doubleMe x + doubleMe y

tripleUs x y = tripleMe x + tripleMe y

tripleMe x = x * 3

doubleSmallNumber x = if x > 100
			then x
			else x * 2

doubleSmallNumber' x = (if x > 100 then x else x * 2) + 1

length' xs = sum [1 | _ <- xs]
