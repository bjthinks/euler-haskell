import Euler

answer :: Int
answer = sum $ map (sum . digits . iSqrt . (*10^198)) $
         filter (paste (/=) id ((^2) . iSqrt)) [1..100]

main :: IO ()
main = putStrLn $ show answer
