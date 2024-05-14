import Euler
import Data.List

values :: [(Int,Int)]
values = [(radical n,n) | n <- [1..100000]]

answer :: Int
answer = snd . (!! 9999) . sort $ values

main :: IO ()
main = putStrLn . show $ answer
