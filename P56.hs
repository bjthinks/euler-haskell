import Euler

answer :: Int
answer = maximum [sum . digits $ a^b | a <- [1..99], b <- [1..99]]

main :: IO ()
main = print answer
