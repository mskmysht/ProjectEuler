collatz :: Int -> [Int]
collatz 1 = [1]
collatz n
  | r == 0    = n : collatz m
  | otherwise = n : collatz (n*3 + 1)
    where
      (m, r) = n `divMod` 2


main = do
  print . maximum . fmap (length . collatz) $ [837799..837799]
