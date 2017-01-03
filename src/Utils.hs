module Utils where

primes :: [Int]
primes = 2 : filter isPrime [3,5..] where
  isPrime n = foldr (\p b -> p * p > n || n `mod` p /= 0 && b) True primes
