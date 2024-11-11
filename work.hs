addAbs :: [Integer] -> Integer
addAbs xs = sum (map abs xs)

main :: IO ()
main = print (addAbs [1, -2, 3, -4])
