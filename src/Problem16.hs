calcdigit :: (Int -> Int -> Int) -> [Int] -> [Int] -> [Int]
calcdigit _ [] [] = []
calcdigit c (m:ms) [] = m : calcdigit c ms []
calcdigit c [] (n:ns) = n : calcdigit c [] ns
calcdigit c (m:ms) (n:ns) = (c m n) : calcdigit c ms ns

sumdigit :: [Int] -> [Int] -> [Int]
sumdigit = calcdigit (+)

carry :: [Int] -> [Int]
carry d = dropWhile (==0) . sumdigit d' $ (0 : m)
  where
    (d', m) = unzip . fmap (\n -> n `divMod` 10) $ d

double :: [Int] -> [Int]
double = carry . fmap (*2)

power :: [Int] -> Int -> [Int]
power _ 0 = [1]
power ns 1 = ns
power ns n = (flip power) (n-1) . double $ ns

main = do
  print . double $ [2]
  print . sum . power [2] $ 1000
