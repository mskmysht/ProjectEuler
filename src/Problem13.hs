calcdigit :: (Int -> Int -> Int) -> [Int] -> [Int] -> [Int]
calcdigit _ [] [] = []
calcdigit c (m:ms) [] = m : calcdigit c ms []
calcdigit c [] (n:ns) = n : calcdigit c [] ns
calcdigit c (m:ms) (n:ns) = (c m n) : calcdigit c ms ns

sumdigit :: [Int] -> [Int] -> [Int]
sumdigit = calcdigit (+)

toDecimal :: Char -> Int
toDecimal c = fromEnum c - fromEnum '0'

carry :: [Int] -> [Int]
carry d = sumdigit d' (0 : m)
  where
    (d', m) = unzip . fmap (\n -> n `divMod` 10) $ d

main = do
  s <- readFile "./150digits.txt"
  let d = foldl sumdigit [] . fmap (fmap toDecimal) $ lines s
  let e = carry . carry . carry . carry $ d
  mapM_ (putStr . show) . take 10 . dropWhile (==0) $ e
