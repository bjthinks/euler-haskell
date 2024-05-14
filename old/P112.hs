import Data.List

isBouncy :: Int -> Bool
isBouncy n = (s /= sort s) && (reverse s /= sort s)
  where
    s = show n

numBouncy :: [(Int,Int)]
numBouncy = numBouncy' 1 0
numBouncy' :: Int -> Int -> [(Int,Int)]
numBouncy' n c
  | isBouncy n = ((c+1),n) : numBouncy' (n+1) (c+1)
  | otherwise = (c,n) : numBouncy' (n+1) c

answer :: Int
answer = head [n | (c,n) <- numBouncy, 100*c >= 99*n]

main :: IO ()
main = putStrLn $ show answer
