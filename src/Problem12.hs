triangles :: Int -> Int
triangles n = sum [1..n]

divisors :: Int -> [Int]
divisors n = filter (\i -> n `mod` i == 0) [1..n]

divcount :: Int -> Int
divcount n = divcount' n 2
  where
    divcount' n' i
      | i * i > n'  = 2
      | i * i == n' = 3
      | otherwise = case length ls of
        1 -> divcount' n' (i+1)
        _ -> if (fst . head $ rs) == 0 then length ls
              else length ls * divcount' (fst . last $ ls) (i+1)
        where
          (ls, rs) = span ((==0) . snd) . iterate (\(m,_) -> m `divMod` i) $ (n', 0)

main = do
  -- very fast
  print . head . filter ((>=500) . divcount) . fmap triangles $ [1..]
  -- very slow
  -- print . head . filter ((>=100) . length . divisors) . fmap triangles $ [1..]

  
  
