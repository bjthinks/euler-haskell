import Euler

quadratics :: [Int -> Int]
quadratics = [\n -> n^2 + a*n + b | a <- [-999..999], b <- [-999..999]]

numPrimes :: (Int -> Int) -> Int
numPrimes f = length $ takeWhile isPrime $ map f [0..]

bestQuadratic :: Int -> Int
bestQuadratic = maximumVia numPrimes quadratics

b :: Int
b = bestQuadratic 0

a :: Int
a = bestQuadratic 1 - b - 1

answer :: Int
answer = a*b

main :: IO ()
main = putStrLn $ show answer
