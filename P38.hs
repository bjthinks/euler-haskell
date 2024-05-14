import Euler
import Data.List

concatenatedProduct :: [Int] -> Int -> Int
concatenatedProduct xs y = undigits . concat . map (digits . (y*)) $ xs

inputs :: [Int]
inputs = map (concatenatedProduct [1,2]) [5000..9999] ++
         map (concatenatedProduct [1,2,3]) [100..333] ++
         map (concatenatedProduct [1,2,3,4]) [25..33] ++
         map (concatenatedProduct [1,2,3,4,5]) [5..9] ++
         map (concatenatedProduct [1,2,3,4,5,6]) [3] ++
         map (concatenatedProduct [1,2,3,4,5,6,7,8,9]) [1]

answer :: Int
answer = maximum . filter ((==[1,2,3,4,5,6,7,8,9]) . sort . digits) $ inputs

main :: IO ()
main = print answer
