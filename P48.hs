import Euler

numbers :: [Modular Integer]
numbers = [(makeModular (10^10) a)^a | a <- [1..1000]]

answer :: Integer
answer = liftPositive (sum numbers)

main :: IO ()
main = print answer
