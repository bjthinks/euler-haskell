module IsPrime (isPrime,iSqrt) where

isPrime :: (Integral a) => a -> Bool
isPrime 0 = False
isPrime 1 = False
isPrime 2 = True
isPrime n
  | n < 0 = isPrime (-n)
  | otherwise = not $ containsZero [n `mod` k | k <- 2:[3,5..iSqrt n]]

containsZero :: (Integral a) => [a] -> Bool
containsZero [] = False
containsZero (0:_) = True
containsZero (_:xs) = containsZero xs

iSqrt :: (Integral a) => a -> a
iSqrt 0 = 0
iSqrt 1 = 1
iSqrt n = head $
          dropWhile (\x -> x*x > n) $
          iterate (\x -> (x + n `div` x) `div` 2) (n `div` 2)
