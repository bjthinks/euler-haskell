import Data.Ratio
import System.IO

digitsGood :: Integer -> Bool
digitsGood 0 = True
digitsGood num
  | mod num 10 <= 2 = digitsGood $ div num 10
  | otherwise = False

fnn :: Integer -> Integer
fnn n = div (head $ filter digitsGood [n,2*n..]) n

progress :: [(Integer,Integer)]
progress = map (\x -> (x,fnn x)) [1..10000]

answer :: Integer
answer = sum $ map snd progress

main :: IO ()
main = do hSetBuffering stdout NoBuffering
          putStrLn $ show progress
          putStrLn $ show answer
