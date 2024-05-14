import Euler

triplets :: [(Int,Int,Int)]
triplets = [(a,b,(1000-a-b)) | a <- [1..500], b <- [1..a-1], a<1000-a-b]

answer :: Int
answer
  | length goodTriplets == 1 = a * b * c
  | otherwise = error "Wrong number of solutions"
    where
      goodTriplets = filter isPythagorean triplets
      (a,b,c) = head goodTriplets

main :: IO ()
main = putStrLn $ show answer
