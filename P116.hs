import Euler

answer :: Integer
answer = (sum . map (!!50) $
          [recurrence [1,1] [1,1],
           recurrence [1,0,1] [1,1,1],
           recurrence [1,0,0,1] [1,1,1,1]]) - 3

main :: IO ()
main = print answer
