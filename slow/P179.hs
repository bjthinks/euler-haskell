import Euler

isGood :: Int -> Bool
isGood = paste (==) numberOfDivisors (numberOfDivisors . (+1))

answer :: Int
answer = length . filter isGood $ 2 : takeWhile (<10^7) [3..]

main :: IO ()
main = print answer
