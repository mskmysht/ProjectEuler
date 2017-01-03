import Data.Monoid
import Data.Foldable (foldMap)

data WdRem = WR { getRem :: Int } deriving Show

instance Monoid WdRem where
  mempty = WR 0
  WR m `mappend` WR n = WR ((m + n) `mod` 7)

countsun :: WdRem -> Sum Int
countsun (WR 6) = Sum 1
countsun _ = Sum 0

leap y
  | y `mod` 4 /= 0 = 0
  | y `mod` 100 == 0 && y `mod` 400 /= 0 = 0
  | otherwise = 1

jan = 31
feb y = 28 + leap y
mar = 31
apr = 30
may = 31
jun = 30
jul = 31
aug = 31
sep = 30
oct = 31
nov = 30
dec = 31

months y = map (WR . (`mod` 7)) $ [
  jan,
  feb y,
  mar,
  apr,
  may,
  jun,
  jul,
  aug,
  sep,
  oct,
  nov,
  dec]

ir = WR 365 <> WR (leap 1900)

main = do
  print $ foldMap countsun
    . scanl (<>) ir
    . concatMap months $ [1901..2000]

