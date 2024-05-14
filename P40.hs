import Euler

-- A terribly inefficient solution, but it takes < 1 second to run

input :: [Int]
input = concat . map digits $ [0..]

answer :: Int
answer = product $ map (input !!) [10^a | a <- [0..6]]

main :: IO ()
main = print answer
