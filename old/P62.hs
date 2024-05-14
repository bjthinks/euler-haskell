import Data.List

cubes :: [Integer]
cubes = [x^3 | x <- [0..]]

sortedCubeStrs :: [String]
sortedCubeStrs = map (sort . show) cubes

permCubeCount :: [Int]
permCubeCount = [length (filter (== sortedCubeStrs !! n) (take (3*n) sortedCubeStrs)) | n <- [0..]]

answer :: Integer
answer = head [(toInteger i)^3 | i <- [0..], permCubeCount !! i == 5]

main :: IO ()
main = putStrLn (show answer)
