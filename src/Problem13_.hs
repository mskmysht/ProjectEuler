data Decimal = Zero
             | One
             | Two
             | Three
             | Four
             | Five
             | Six
             | Seven
             | Eight
             | Nine  deriving (Show, Enum)

data Counter = Counter {
                 _1 :: Int
                ,_2 :: Int
                ,_3 :: Int
                ,_4 :: Int
                ,_5 :: Int
                ,_6 :: Int
                ,_7 :: Int
                ,_8 :: Int
                ,_9 :: Int
               } deriving Show

counter = Counter 0 0 0 0 0 0 0 0 0

getter :: Decimal -> Counter -> Int
getter Zero  = \_ -> 0
getter One   = _1
getter Two   = _2
getter Three = _3
getter Four  = _4
getter Five  = _5
getter Six   = _6
getter Seven = _7
getter Eight = _8
getter Nine  = _9

setter :: Decimal -> Counter -> Int -> Counter
setter Zero  c _ = c
setter One   c n = c { _1 = n}
setter Two   c n = c { _2 = n}
setter Three c n = c { _3 = n}
setter Four  c n = c { _4 = n}
setter Five  c n = c { _5 = n}
setter Six   c n = c { _6 = n}
setter Seven c n = c { _7 = n}
setter Eight c n = c { _8 = n}
setter Nine  c n = c { _9 = n}

increase :: Decimal -> Counter -> Counter
increase d c = setter d c (getter d c + 1)

-- zero = Dec 0 0 0 0 0 0 0 0 0
-- data Digit = Dig { f :: (Decimal -> Int), d :: Decimal }
-- one   = Dig _1 zero {_1 = 1}
-- two   = Dig _2 zero {_2 = 1}
-- three = Dig _3 zero {_3 = 1}
-- four  = Dig _4 zero {_4 = 1}
-- five  = Dig _5 zero {_5 = 1}
-- six   = Dig _6 zero {_6 = 1}
-- seven = Dig _7 zero {_7 = 1}
-- eight = Dig _8 zero {_8 = 1}
-- nine  = Dig _9 zero {_9 = 1}

toDecimal :: Char -> Decimal
toDecimal c = toEnum $ fromEnum c - fromEnum '0' :: Decimal

-- toDigit '1' = Just one
-- toDigit '2' = Just two
-- toDigit '3' = Just three
-- toDigit '4' = Just four
-- toDigit '5' = Just five
-- toDigit '6' = Just six
-- toDigit '7' = Just seven
-- toDigit '8' = Just eight
-- toDigit '9' = Just nine
-- toDigit _   = Nothing

main = do
  s <- readFile "./150digits.txt"
  let c = foldl (\c d -> increase d c) counter . fmap toDecimal $ id =<< lines s
  print c


