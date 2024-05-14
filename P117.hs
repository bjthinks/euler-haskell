import Euler

answer :: Integer
answer = (recurrence [1,1,1,1] [1,1,2,4]) !! 50

main :: IO ()
main = print answer
