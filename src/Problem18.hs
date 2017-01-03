import Prelude hiding (replicate, maximum)
import qualified Data.List as L hiding (replicate, maxiumum)
import Data.Vector hiding (head)

type PL = (Int, Int, Int)

longestPaths t = calc t [(0,0, head . head $ t)] vs
  where
    vs = (replicate (L.sum . L.map L.length $ t) 0)

calc :: [[Int]] -> [PL] -> Vector Int -> Vector Int
calc _ [] ls = ls
calc t ((i,j,v):ps) ls
  | i == L.length t - 1 = calc t ps ls
  | otherwise     =
    let
      (ls1, m1) = f (i+1, j) ls
      (ls2, m2) = f (i+1, j+1) ls1
      ps' = ins m2 . ins m1 $ ps
    in
      calc t ps' ls2

  where
    -- map f . idx $ [(i+1,j), (i+1,j+1)]
    -- v = t ! $ idx i j
    -- (i, j, v) = L.head ps
    ins :: Maybe PL -> [PL] -> [PL]
    ins Nothing = id
    ins (Just p) = 
      L.insertBy (\(_,_,l) (_,_,m) -> if l > m then LT else GT) p

    f (i,j) ls
      | l > l' = (ls, Nothing)
      | otherwise = (ls // [(n, l')], Just (i,j,l'))
      where
        l' = v + w
        n = idx i j
        l = ls ! n
        w = (t !! i) !! j
    
    idx :: Int -> Int -> Int
    idx i j = i*(i+1) `div` 2 + j

main = do
  s <- readFile "triangle.txt"
  let t = fmap (fmap (read :: String -> Int) . words) . lines $ s
  print t
  let ls = longestPaths t
  print $ maximum ls
