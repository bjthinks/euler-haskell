import Euler
import Data.Ratio

cfracE :: [Integer]
cfracE = 2 : concat [[1,k,1] | k <- [2,4..]]

answer :: Int
answer = sum $ digits $ numerator $ convergents cfracE !! 99

main :: IO ()
main = putStrLn $ show answer
