--Зад.1 done
hailstone :: Integral a => a -> [a]
hailstone n
   | n == 1 = [1]
   | odd n = [n] ++ hailstone(3*n+1)
   | otherwise = [n] ++ hailstone(n `div` 2)

--Зад.2 done
prime :: Integral a => a -> Bool
prime 1 = False
prime n = null [ d | d <- [2..(n-1)], mod n d == 0 ]
primesUntil100 = [x | x <- [2..100], prime x]
squares = [x^2 | x <-[2..7]]
numsCan = [x+y | x <- primesUntil100, y <- squares, x+y < 100 , odd (x+y) ]
final = length $ filter (`notElem` numsCan) [1,3..100]

--Зад.3
--see again later
primes = [x | x <- [2..],prime x]
divisors n
   | prime n = [(n,1)]
   | n == 1 = []

test n
   | n == 3 = []
   | otherwise = pr : test (n + 1)
     where pr = take primes

powers n pr 
   | prime n = 
   | n `mod` pr == 0 = pr : powers (n `div` pr) pr
   | otherwise = []

--Зад.4 done
-- само може би да се напише типа на функцията
intercalate' x  (y : ys) 
   | length ys == 0 = y 
   | otherwise = y ++ x ++ intercalate' x ys

--Зад.6
helper [] = []
helper (x:xs) = [(x,head x,last x)] ++ helper xs
