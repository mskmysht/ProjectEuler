primes :: [Int]
primes = 2 : filter isPrime [3,5..] where
  isPrime n = foldr (\p b -> p * p > n || n `mod` p /= 0 && b) True primes

judge :: Int -> [Int] -> Int
judge n pps@(p:ps) | n == p || p * p > n = n
                   | n `mod` p == 0 = judge (n `div` p) pps
                   | otherwise = judge n ps

main = do
  putStr . show $ judge 600851475143 primes
