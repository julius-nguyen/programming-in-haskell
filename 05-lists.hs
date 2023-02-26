module Lists where

import Prelude hiding (replicate)

-- sum of the squares of integers up to hundred
hundredSquares :: Int 
hundredSquares = sum [n^2 | n <- [1..100]]

-- creates a list of pairs (x,y) of integers 
-- such that 0 <= x <= m and 0 <= x <= n
grid :: Int -> Int -> [(Int,Int)]
grid m n = [(x,y) | x <- [0..m],y <- [0..n]]

-- creates a list of coordinate squares 
-- excluding diagonal cells (0,0) to (n,n)
square :: Int -> [(Int,Int)]
square n = [(x,y) | (x,y) <- grid n n, x /= y]

replicate :: Int -> a -> [a]
replicate n x = [x | _ <- [1..n]]

-- returns list of three integers which satisfy
-- Pythagoras' theorem
pyths :: Int -> [(Int,Int,Int)]
pyths n = [(x,y,z) | x <- [1..n],y <- [1..n], z <- [1..n], x^2+y^2==z^2]

factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

prime :: Int -> Bool
prime n = factors n == [1,n]

primes :: Int -> [Int]
primes n = [x | x <- factors n, prime x]

find :: Eq a => a -> [(a,b)] -> [b]
find k t = [v | (k',v) <- t, k == k']

pairs :: [a] -> [(a,a)]
pairs xs = zip xs (tail xs)

sorted :: Ord a => [a] -> Bool
sorted xs = and [x <= y | (x,y) <- pairs xs]
    
perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], x == sum (init (factors x)) ]

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x',i) <- zip xs [0..], x == x']

-- positions :: Eq a => a -> [a] -> [Int]
-- positions x xs = [i | i <- find x (zip xs [0..])]

lowers :: String -> Int 
lowers xs = length [x | x <- xs, x >= 'a' && x <= 'z'] 

count :: Char -> String -> Int 
count x xs = length [x' | x' <- xs, x == x']


scalarproduct :: [Int] -> [Int] -> Int 
scalarproduct xs ys = sum [x*y | (x,y) <- zip xs ys]
