module Person
( Person
, firstName
, lastName
, age
, height
, phoneNumber
, flavor
) where

data Person = Person { firstName :: String
		     , lastName :: String
		     , age :: Int
		     , height :: Float
		     , phoneNumber :: String
		     , flavor :: String
		     } deriving (Eq, Show)

