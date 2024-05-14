import Euler

answer :: Int
answer = fst $ head $ dropWhile ((<10^999) . snd) $ zip [0..] fibonacci

main :: IO ()
main = putStrLn $ show answer
