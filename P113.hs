import Euler

d :: Integer
d = 100

answer :: Integer
answer = choose (9+d) 9 + choose (10+d) 10 - 10*d - 2

main :: IO ()
main = print answer
