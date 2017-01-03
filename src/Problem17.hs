data Numbers = Numbers { count :: Int, letters :: Int } deriving Show

numbers :: [String] -> Numbers
numbers ss = Numbers (length ss) (sum . fmap length $ ss)

(^+^) (Numbers c1 l1) (Numbers c2 l2) = Numbers (c1 + c2) (l1 + l2)
(^*^) (Numbers c1 l1) (Numbers c2 l2) = Numbers (c1 * c2) (l1 * c2 + c1 * l2)

empn = numbers [""]

_1n = numbers ["one"]

_2_9 = ["two",
        "three",
        "four",
        "five",
        "six",
        "seven",
        "eight",
        "nine"]
_1_9n = _1n ^+^ numbers _2_9

_10_19 = ["ten",
          "eleven",
          "twelve",
          "thirteen",
          "fourteen",
          "fifteen",
          "sixteen",
          "seventeen",
          "eighteen",
          "nineteen"]
_10_19n = numbers _10_19

_20_90 = ["twenty",
          "thirty",
          "forty",
          "fifty",
          "sixty",
          "seventy",
          "eighty",
          "ninety"]
_20_90n = numbers _20_90

_100 = ["hundred"]
_100n = numbers _100

andn = numbers ["and"]

_1000n = numbers ["thousand"]


_1_99n = _20_90n ^*^ (empn ^+^ _1_9n) ^+^ _10_19n ^+^ _1_9n

main = do
  print $ _1_9n ^*^ _100n ^*^ (empn ^+^ (andn ^*^ _1_99n)) ^+^ _1_99n ^+^ (_1n ^*^ _1000n)
