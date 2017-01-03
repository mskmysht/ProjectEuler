import Utils

main :: IO ()
main = do
  print $ primes !! (10001 - 1)
