import Data.Ratio
import Data.List

fracs :: [Ratio Int]
fracs = [n % d | d <- [4..12000],
                 n <- [(div d 3) + 1 .. -(div (-d) 2) - 1],
                 denominator (n%d) == d]

answer :: Int
answer = length fracs

main :: IO ()
main = putStrLn $ show answer
