import Euler

pathLengths :: (Int,Int,Int) -> [Int]
pathLengths (a,b,c) = [a^2+(b+c)^2,b^2+(a+c)^2,c^2+(a+b)^2]

isSquare :: Int -> Bool
isSquare n = n == (iSqrt n)^2

cuboidsOfSize :: Int -> [(Int,Int,Int)]
cuboidsOfSize m = [(a,b,m) | a <- [1..m], b <- [a..m]]

solutionsOfSize :: Int -> Int
solutionsOfSize = length . filter isSquare . map (minimum . pathLengths) .
                  cuboidsOfSize

findit :: [(Int,Int)] -> Int
findit xs = findit' 0 xs
  where
    findit' t ((n,s):xs)
      | t+s <= 32000 = findit' (t+s) xs
      | otherwise = n

answer :: Int
answer = findit $ map (\n -> (n,solutionsOfSize n)) [1..]

main :: IO ()
main = print answer
