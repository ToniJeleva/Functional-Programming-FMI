saySign :: Int -> String  
saySign n  
    | n == 0 = "Zero"  
    | n < 0 = "Negative"  
    | n > 0 = "Positive"  

useless 0 _ _ _ = 0
useless _ 0 _ _ = 0
useless _ _ 0 _ = 0
useless _ _ _ 0 = 0
useless a b c d = a + b + c +d

modulus :: Floating a => (a,a) -> a
modulus (x,y) = sqrt (x^2 + y^2)

complAdd :: Floating a => (a,a) -> (a,a) -> (a,a)
complAdd (x1,y1) (x2,y2) = ((x1 + x2),(y1 + y2)) 

ackerman m n 
	| m == 0          = n + 1
	| m > 0 && n == 0 = ackerman (m-1) 1
	| m > 0 && n > 0  = ackerman (m-1) (ackerman m (n - 1))

distance (x1,y1) (x2,y2) = sqrt ((x1 - x2)^2 + (y1 - y2)^2)

replicate' num 0 = [] 
replicate' num times = num : (replicate' num (times - 1))

take' 0  ls = []
take' n ls = head ls : (take' (n - 1) (drop 1 ls))

prime 1 = False
prime n = null [ d | d <- [2..(n -1)], mod n d == 0]

primes = [ x | x <- [2..], prime x]

nthprime :: Int -> Int
nthprime n = primes !! n

removeNth _ [] = []
removeNth n lst = take n lst ++ (removeNth n (drop (n + 1) lst))