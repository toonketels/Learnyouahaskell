import Data.Char

main = do
	putStrLn "Hello, what is your firstname?"
	firstName <- getLine
	putStrLn "And your lastname?"
	lastName <- getLine
	let bigFirstName = map toUpper firstName
	    bigLastName = map toUpper lastName
	putStrLn $ "Hey " ++ bigFirstName ++ " " ++ bigLastName ++ ", how are you?"
