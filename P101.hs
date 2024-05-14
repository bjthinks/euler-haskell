import Data.Ratio

-- 7 -> [2,4] -> a quadratic polynomial through the points (7,1), (2,0), (4,0)
lagrangeBasisPolynomial :: Fractional a => a -> [a] -> a -> a
lagrangeBasisPolynomial y zs x = product [(x - z) / (y - z) | z <- zs]

lagrangeInterpolationPolynomial :: Fractional a => [(a,a)] -> a -> a
lagrangeInterpolationPolynomial points z =
  sum [y * lagrangeBasisPolynomial x (map fst (filter (/=(x,y)) points)) z |
       (x,y) <- points]

type Rat = Ratio Integer

u :: Rat -> Rat
u n = 1 - n + n^2 - n^3 + n^4 - n^5 + n^6 - n^7 + n^8 - n^9 + n^10

points :: [(Rat,Rat)]
points = [(x,u x) | x <- [1..]]

bops :: [Rat -> Rat]
bops = map lagrangeInterpolationPolynomial [take k points | k <- [1..10]]

fit :: (Rat -> Rat) -> Rat
fit bop = fst . head . filter (uncurry (/=)) $ [(bop x, u x) | x <- [1..]]

answer :: Rat
answer = sum . map fit $ bops

main :: IO ()
main = do case denominator answer of
            1 -> print $ numerator answer
            _ -> error "answer is not an integer"

