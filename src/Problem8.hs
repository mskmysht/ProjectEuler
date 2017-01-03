import Data.Traversable
import Data.List
import Data.Ord

thds = "73167176531330624919225119674426574742355349194934" ++
       "96983520312774506326239578318016984801869478851843" ++
       "85861560789112949495459501737958331952853208805511" ++
       "12540698747158523863050715693290963295227443043557" ++
       "66896648950445244523161731856403098711121722383113" ++
       "62229893423380308135336276614282806444486645238749" ++
       "30358907296290491560440772390713810515859307960866" ++
       "70172427121883998797908792274921901699720888093776" ++
       "65727333001053367881220235421809751254540594752243" ++
       "52584907711670556013604839586446706324415722155397" ++
       "53697817977846174064955149290862569321978468622482" ++
       "83972241375657056057490261407972968652414535100474" ++
       "82166370484403199890008895243450658541227588666881" ++
       "16427171479924442928230863465674813919123162824586" ++
       "17866458359124566529476545682848912883142607690042" ++
       "24219022671055626321111109370544217506941658960408" ++
       "07198403850962455444362981230987879927244284909188" ++
       "84580156166097919133875499200524063689912560717606" ++
       "05886116467109405077541002256983155200055935729725" ++
       "71636269561882670428252483600823257530420752963450"

s2ds :: String -> [Int]
s2ds = fmap (\c -> fromEnum c - fromEnum '0')

thd = s2ds thds

findIndexOf :: Eq a => a -> [a] -> [Int]
findIndexOf e [] = []
findIndexOf e as = f (-1) as where
  f i as | rs == []  = [i, l]
         | otherwise = i : (f (i + length ls + 1) (tail rs)) where
    (ls, rs) = break (== e) as
  l = length as

indexWithDists :: [Int] -> [(Int, Int)]
indexWithDists [] = []
indexWithDists is = fmap (\(i, j) -> (i + 1, j - i - 1)) $ is `zip` (tail is)

indexWithDistsOf :: Eq a => a -> [a] -> [(Int, Int)]
indexWithDistsOf e = indexWithDists . findIndexOf e

distsOf :: Eq a => a -> [a] -> [Int]
distsOf e [] = []
distsOf e ns = reverse $ distsOf' ns where
  -- distsOf' :: Eq a => [a] -> [Int]
  distsOf' ns | length rs == 0 = []
              | otherwise       = (length ls + 1) : (distsOf' $ tail rs) where
    (ls, rs) = break (== e) ns

withIndex :: [Int] -> [(Int, Int)]
withIndex = tail . scanl (\(j, _) l -> (j + l, l)) (-1, 0)

indexWith :: ([a] -> a) -> Int -> [a] -> [(Int, a)]
indexWith f n ns = [0..(length ns - n)] >>= \i -> do
  let s = f . take n . drop i $ ns
  return (i, s)

indexWithSums = indexWith sum

maxs :: [(Int, Int)] -> [(Int, Int)]
maxs ps = maxs' [] ps where
  maxs' :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
  maxs' ms [] = ms
  maxs' [] (p:ps) = maxs' [p] ps
  maxs' ms@(m:_) (p:ps)
    | s == snd p = maxs' (p : ms) ps
    | s > snd p  = maxs' ms ps
    | otherwise  = maxs' [p] ps where
      s = snd m

main = do
  let n = 13
      ids = filter (\(i, l) -> l >= n) . indexWithDistsOf '0' $ thds
      ms = maxs $ ids >>= \(i, l) -> do
        let ss = take l . drop i $ thds
        fmap (\(j, s) -> (i + j, s)) .  maxs . indexWith product n . s2ds $ ss
  print ms
