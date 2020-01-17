module Value where

type ParameterList = [(String, Double)]

data Value = Value Double
  | Parameter String Double
  | Relation (ParameterList -> Double)

instance Eq Value where
  (Value d1) == (Value d2) = d1 == d2
  (Parameter n1 _) == (Parameter n2 _) = n1 == n2
  _ == _ = False

instance Show Value where
  show (Value d) = show d
  show (Parameter n d) = show n ++ "(" ++ show d ++ ")"
  show (Relation _) = "complex value"

liftValue :: (Double -> Double) -> (Value -> Value)
liftValue f (Value d) = Value $ f d
liftValue f v = Relation (\pl -> f $ extract pl v)

liftValue2 :: (Double -> Double -> Double) -> (Value -> Value -> Value)
liftValue2 f (Value d1) (Value d2) = Value (f d1 d2)
liftValue2 f v1 v2 = Relation (\pl -> f (extract pl v1) (extract pl v2))

instance Num Value where
  (+) = liftValue2 (+)
  (*) = liftValue2 (*)
  abs = liftValue abs
  signum = liftValue signum
  fromInteger = Value . fromInteger
  negate = liftValue negate

instance Fractional Value where
  fromRational = Value . fromRational
  (/) = liftValue2 (/)

instance Floating Value where
  pi = Value pi
  exp = liftValue exp
  log = liftValue log
  sin = liftValue sin
  cos = liftValue cos
  asin = liftValue asin
  acos = liftValue acos
  atan = liftValue atan
  sinh = liftValue sinh
  cosh = liftValue cosh
  asinh = liftValue asinh
  acosh = liftValue acosh
  atanh = liftValue atanh

extract :: ParameterList -> Value -> Double
extract _ (Value d) = d
extract [] (Parameter _ dv) = dv
extract ((n,v):t) p@(Parameter pn _)
  | pn == n = v
  | otherwise = extract t p
extract pl (Relation f) = f pl
