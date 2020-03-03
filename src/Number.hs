module Number where

type NumberList = [(String, Int)]

data Number = Number Int
  | XNumber String Int
  | FNumber (NumberList -> Int)

instance Show Number where
  show (Number i) = show i
  show (XNumber n i) = show n ++ "(" ++ show i ++ ")"
  show (FNumber _) = "complex number"

--TODO: liftParameter для Value и Number
liftNumber :: (Int -> Int) -> (Number -> Number)
liftNumber f (Number i) = Number $ f i
liftNumber f v = FNumber (\pl -> f $ extractNumber pl v)

liftNumber2 :: (Int -> Int -> Int) -> (Number -> Number -> Number)
liftNumber2 f (Number i1) (Number i2) = Number (f i1 i2)
liftNumber2 f v1 v2 = FNumber (\pl -> f (extractNumber pl v1) (extractNumber pl v2))

instance Num Number where
  (+) = liftNumber2 (+)
  (*) = liftNumber2 (*)
  abs = liftNumber abs
  signum = liftNumber signum
  fromInteger = Number . fromInteger
  negate = liftNumber negate

extractNumber :: NumberList -> Number -> Int
extractNumber _ (Number d) = d
extractNumber [] (XNumber _ dv) = dv
extractNumber ((n,v):t) p@(XNumber pn _)
  | pn == n = v
  | otherwise = extractNumber t p
extractNumber pl (FNumber f) = f pl
