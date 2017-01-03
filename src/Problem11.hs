import Data.Matrix
import qualified Data.Vector as V

intGrid :: [Char] -> [[Int]]
intGrid = fmap (fmap read .  words) . lines

toMatrix :: String -> Matrix Int
toMatrix = fromLists . intGrid

size = 4 :: Int

subs :: Matrix a -> [Matrix a]
subs m = do
    i <- [1..nrows m - size + 1]
    j <- [1..ncols m - size + 1]
    return $ submatrix i (i+size-1) j (j+size-1) m

nozero :: Matrix Int -> Bool
nozero = any (==0) . toList

products :: Matrix Int -> [Int]
products m = fmap product $ r : d : cs ++ rs
  where
    rs = fmap (\i -> getRow i m) [1..nrows m]
    cs = fmap (\i -> getCol i m) [1..ncols m]
    d  = getDiag m
    r  = V.fromList $ fmap (\i -> getElem i (n - i + 1) m) [1..n]
    n = min (nrows m) (ncols m)

main = do
  s <- readFile "./20-20grid.txt"
  let m = toMatrix $ s
  print . maximum . fmap (maximum . products) . subs $ m
