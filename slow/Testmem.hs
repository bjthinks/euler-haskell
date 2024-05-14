import Data.Array

memoize :: (Ix a, Enum a) => (a -> b) -> a -> a -> a -> b
memoize f lo hi x
  | lo <= x && x <= hi = table ! x
  | otherwise = f x
    where
      table = array (lo,hi) [(x,f x) | x <- [lo..hi]]

slow :: Integer -> Integer
slow x = sum [3000000*x..3000000*(x+1)]

mslow :: Integer -> Integer
mslow = memoize slow 1 5

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

mfib :: Integer -> Integer
mfib = memoize fib 0 5

main :: IO [()]
main = sequence [
  putStrLn $ show $ slow 2,
  putStrLn $ show $ slow 2,
  putStrLn $ show $ slow 2,
  putStrLn $ show $ mslow 3,
  putStrLn $ show $ mslow 3,
  putStrLn $ show $ mslow 3
  ]
