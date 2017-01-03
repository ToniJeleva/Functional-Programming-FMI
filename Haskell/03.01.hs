sumProducts ll = sum (map product ll)

occurrences l1 l2 = map (\e -> times e l2) l1
times x ls = length (filter (\y -> x == y) ls)

transpose m
	| null (head m) = []
	| otherwise     = (firstCol m) : transpose (removeFirstCol m)
	where firstCol m = map head m
	      removeFirstCol m = map tail m

mainDiag m
	| null m = []
	| otherwise = (head (head m)) : mainDiag(tail (map tail m))

secondDiag m = mainDiag (map reverse m)

isSquare ls = all id (map (\y -> length ls == y ) lengths)	
	where lengths = map length ls

--работим със сортирани множества
setUnion s1 [] = s1
setUnion [] s2 = s2
setUnion (x:xs)(y:ys)
	| x < y = x : setUnion xs (y:ys)
	| x == y = x : setUnion xs ys
	| x > y = x : setUnion (x:xs) ys


setIntersect s1 [] = []
setIntersect [] s2 = []
setIntersect (x:xs) (y:ys)
	| x < y  = setIntersect xs (y:ys)
	| x == y = x : setIntersect xs ys
	| x > y  = setIntersect (x:xs) ys

setDiff s1 [] = []
setDiff [] s2 = []
setDiff (x:xs)(y:ys)
	| x < y  = x : setDiff xs (y:ys)
	| x == y = setDiff xs ys
	| x > y  = setDiff (x:xs) ys
