s100 = sum [1..100]
ans = foldr (\i s -> i * (s100 - i) + s) 0 [1..100]

main = do
  putStr . show $ ans
