sumdigit :: [Int] -> [Int] -> [Int]
sumdigit [] _ = []
sumdigit _ [] = []
sumdigit (m:ms) (n:ns) = (m + n) : sumdigit ms ns

toDecimal :: Char -> Int
toDecimal c = fromEnum c - fromEnum '0'

main = do
  s <- readFile "./150digits.txt"
  let c = foldl sumdigit (repeat 0) . fmap (fmap toDecimal) $ lines s
  print c
  print $ length c


