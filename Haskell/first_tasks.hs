--hello n-times
hello_worlds 0 = return ()
hello_worlds x = do putStrLn "Hello World"
                    hello_worlds (x-1)

--repilicate every element n times
f1 :: Int -> [Int] -> [Int]
f1 n arr = do
      a  <- arr
      replicate n a

--only elements smaller than n
f :: Int -> [Int] -> [Int]
f n arr = [x | x <-arr, x < n]


-- reverse list
rev [] = []
rev (x:xs) = rev xs ++ [x]

--list length
len :: [a] -> Int
len lst = sum[1|_ <- lst] 
