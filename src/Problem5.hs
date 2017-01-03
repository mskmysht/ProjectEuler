-- problem 5
remainders = map (\i -> (2520 `mod` i,i)) [1..20]
ans = 2520 * 11 * 13 * 17 * 19 * 2
check = sum . map (\i -> (ans `mod` i)) $ [1..20]

main = do
  putStrLn . show $ ans
  putStrLn . show $ check
