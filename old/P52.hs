import Data.List

allEqual :: [String] -> Bool
allEqual (x:y:zs)
  | x == y = allEqual (y:zs)
  | otherwise = False
allEqual _ = True

main :: IO ()
main = putStrLn $ show $ fst $ head $ filter snd $
       map (\x -> (x, allEqual $ map (sort . show) [2*x,3*x,4*x,5*x,6*x]))
       [1..]
