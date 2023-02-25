module Functions  where 

import Prelude hiding (even, signum, (&&), fst, snd, head)

even :: Integral a => a -> Bool
even n = n `mod` 2 == 0

abs1 :: Int -> Int 
abs1 n = if n >= 0 then n else -n

abs2 :: Int -> Int 
abs2 n | n >= 0 = n
       | otherwise = -n

signum n | n < 0 = -1
         | n == 0 = 0
         | n > 0 = 1

(&&) :: Bool -> Bool -> Bool 
True && b = b 
False & _ = False 

fst :: (a,b) -> a 
fst (x,_) = x

snd :: (a,b) -> b 
snd (_,y) = y

head :: [a] -> a 
head (x:_) = x

rev :: [a] -> [a]
rev [] = []
rev (x:xs) = reverse xs ++ [x]

add :: Int -> Int -> Int 
add = \x -> \y -> x + y

odds :: Int -> [Int]
odds n = map (\x -> x*2+1) [0..n-1]

len :: [a] -> Int 
len [] = 0
len (x:xs) = 1 + len xs

halve :: [a] -> ([a],[a])
halve xs = (take mid xs, drop mid xs)
            where mid = len xs `div` 2

safetail1 :: Eq a => [a] -> [a]
safetail1 xs = if isemptylist xs then [] else xx
    where (x:xx) = xs

safetail2 :: Eq a => [a] -> [a]
safetail2 (x:xs) | isemptylist (x:xs) == True = []
                 | otherwise = xs

isemptylist :: [a] -> Bool 
isemptylist [] = True 
isemptylist _ = False 
