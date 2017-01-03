import Data.List
import Control.Monad

d3 = [999,998..100]
n2dr :: Int -> [Int]
n2dr n
  | n < 10 = [n]
  | otherwise = d : n2dr (n `div` 10) where
    d = n `mod` 10

main = do
 let ans = fmap maximum
             . sequence
             . filter (/= Nothing)
             . fmap (find (\n -> let d = n2dr n in reverse d == d))
             $ (\ds -> if ds == [] then []; else map (* head ds) ds)
             <$> tails d3
 print ans
