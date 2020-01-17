module Value where

type ParameterList = [(String, Double)]

data Value = Value Double
  | Parameter String Double
  | Relation (ParameterList -> Double)

extract :: ParameterList -> Value -> Double
extract _ (Value d) = d
extract [] (Parameter _ dv) = dv
extract ((n,v):t) p@(Parameter pn _)
  | pn == n = v
  | otherwise = extract t p
extract pl (Relation f) = f pl
