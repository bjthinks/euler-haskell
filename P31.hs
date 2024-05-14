ways :: Integer -> [Integer] -> Integer
ways _ [] = 0
ways 0 _ = 1
ways n (c:cs)
  | n >= 0 = ways (n-c) (c:cs) + ways n cs
  | otherwise = 0

main :: IO ()
main = putStrLn $ show $ ways 200 [200,100,50,20,10,5,2,1]
