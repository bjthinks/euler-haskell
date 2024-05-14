import Euler

isBouncy :: Int -> Bool
isBouncy n = not (isNonDecreasing dn || isNonIncreasing dn)
  where
    dn = digits n

isNonDecreasing :: [Int] -> Bool
isNonDecreasing [] = True
isNonDecreasing [_] = True
isNonDecreasing (x:y:zs) = x <= y && isNonDecreasing (y:zs)

isNonIncreasing :: [Int] -> Bool
isNonIncreasing = isNonDecreasing . map (9-)

blet :: [(Int,Int)]
blet = b (1,0)
  where
    b (n,k) = (n,k) : case isBouncy (n+1) of
      True -> b (n+1,k+1)
      False -> b (n+1,k)

answer :: Int
answer = fst . head . filter (\(n,k) -> 99*n==100*k) $ blet

main :: IO ()
main = print answer
