type Tri = (Int, Int, Int)

cs :: [Int]
cs = filter (\n -> n `mod` 4 == 1) [334 .. 997]

toTriple :: [Int] -> [[Tri]]
toTriple = fmap (\ci -> [(ai, 1000 - ai - ci,  ci) | ai <- [1 .. ((1000 - ci) `div` 2)]])

bs :: [Tri] -> [Tri]
bs = filter (\(ai, bi, ci) -> ai^2 + bi^2 == ci^2)

abc :: Tri -> Int
abc (a,b,c) = a * b * c

main = do
  print . fmap abc . concat . fmap bs $ toTriple cs
