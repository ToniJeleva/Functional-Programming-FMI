import Data.List

--Зад.1 
hailstone :: Integral a => a -> [a]
hailstone n
   | n == 1 = [1]
   | odd n = [n] ++ hailstone(3*n+1)
   | otherwise = [n] ++ hailstone(n `div` 2)

--Зад.2 
prime :: Integral a => a -> Bool
prime 1 = False
prime n = null [ d | d <- [2..(n-1)], mod n d == 0 ]
primesUntil n = [x | x <- [2..n], prime x]

squares = [x^2 | x <-[2..7]]
numsCan = [x+y | x <- primesUntil 100, y <- squares, x+y < 100 , odd (x+y) ]
final = length $ filter (`notElem` numsCan) [1,3..100]

--Зад.3 
divisors :: Integral a => a -> [(a, Int)]
divisors n
   | prime n = [(n,1)]
   | n == 1 = []
   | otherwise = filter (\(_,y) -> y /= 0) (map (\prime -> power n prime) (primesUntil n))

power :: Integral a => a -> a -> (a, Int)
power n divisor = (divisor, length $ times n divisor)

times :: (Integral a) => a -> a -> [a]
times n divisor
   | n `mod` divisor == 0 = 1 : times (n `div` divisor) divisor
   | otherwise = []

--Зад.4 
intercalate' :: [a] -> [[a]] -> [a]
intercalate' x  (y : ys) 
   | length ys == 0 = y 
   | otherwise = y ++ x ++ intercalate' x ys


 