import Euler

nonSquares :: [Int]
nonSquares = filter (not . equal id ((^2) . iSqrt)) [1..10000]

repeatingLengths :: [Int]
repeatingLengths = map (length . snd . cfracSqrt) nonSquares

answer :: Int
answer = length $ filter isOdd repeatingLengths

main :: IO ()
main = putStrLn $ show answer
