zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f ls1 ls2 = f (head ls1) (head ls2) : (zipWith f (drop 1 ls1) (drop 1 ls2))

flip' f x y = f y x

takeWhile' _ [] = []
takeWhile' p (x:xs)
	| p x = x : takeWhile' p xs
	| otherwise = []

compress [] = []
compress lst = ((head lst), firsts) : compress rest
	where firsts = length (takeWhile (\x -> x == (head lst)) lst)
		  rest = dropWhile (\x -> x == (head lst)) lst