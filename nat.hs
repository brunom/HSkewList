{-# LANGUAGE DataKinds, TypeOperators, GADTs, TypeFamilies, RecordWildCards #-}

-- Unary naturals
data Unary = Z | S Unary

-- GADT unary naturals
data GUnary :: Unary -> * where
  GZero :: GUnary Z
  GSucc :: GUnary n -> GUnary (S n)

-- GADT unary shared naturals
data GSUnary :: Unary -> * where
 GSZero :: {succZ :: GSUnary (S Z)} -> GSUnary Z
 GSSucc :: {gspred :: GSUnary a, succS :: GSUnary (S (S a))} -> GSUnary (S a)

gssucc :: GSUnary a -> GSUnary (S a)
gssucc GSZero{..} = succZ
gssucc GSSucc{..} = succS

gszero = GSZero { succZ = gsbuild gszero }
gsbuild :: GSUnary n -> GSUnary (S n)
gsbuild pred = node
  where node = GSSucc {gspred = pred, succS = gsbuild node}

-- Segmented binary naturals
segSucc []                   = ((Z, Z):[])
segSucc ((Z,n):[])           = ((S n, Z):[])
segSucc ((Z,n):(Z,m):ns)     = ((S n, S m):ns)
segSucc ((Z,n):(S x, m):ns)  = ((S n, Z):(x, m):ns)
segSucc ((S Z, n):ns)        = ((Z, S n):ns)
segSucc ((S (S n), m):ns)    = ((Z, Z):(n, m):ns)

segPred ((Z, Z):[])          = []
segPred ((S n, Z):[])        = ((Z,n):[])
segPred ((S n, S m):ns)      = ((Z,n):(Z,m):ns)
segPred ((S n, Z):(x, m):ns) = ((Z,n):(S x, m):ns)
segPred ((Z, S n):ns)        = ((S Z, n):ns)
segPred ((Z, Z):(n, m):ns)   = ((S (S n), m):ns)

-- Skew binary naturals
skewSucc []           = [Z]
skewSucc [d]          = [Z,d]
skewSucc (d:Z:ds)     = (S d:ds)
skewSucc (d1:S d2:ds) = (Z:d1:d2:ds)

skewPred [Z]          = []
skewPred [Z,d]        = [d]
skewPred (S d:ds)     = (d:Z:ds)
skewPred (Z:d1:d2:ds) = (d1:S d2:ds)

-- Type level skew binary naturals
type family HSkewSucc (a :: [Unary]) :: [Unary] where
  HSkewSucc '[]                = '[Z]
  HSkewSucc '[d]               = '[Z,d]
  HSkewSucc (d ':Z ':ds)       = (S d ':ds)
  HSkewSucc (d1 ':S d2 ':ds)   = (Z ':d1 ':d2 ':ds)

type family HSkewPred (a :: [Unary]) :: [Unary] where
  HSkewPred '[Z]               = '[]
  HSkewPred '[Z,d]             = '[d]
  HSkewPred (S d ':ds)         = (d ':Z ':ds)
  HSkewPred (Z ':d1 ':d2 ':ds) = (d1 ':S d2 ':ds)

-- Length indexed lists
data Vec :: * -> Unary -> * where
  VecNil :: Vec a Z
  VecCons :: a -> Vec a n -> Vec a (S n)

-- GADT unary ordinals
data OUnary :: Unary -> * where
  OZero :: OUnary (S n)
  OSucc :: OUnary n -> OUnary (S n)

unaryNth :: Vec a n -> OUnary n -> a
unaryNth (VecCons a _) OZero = a
unaryNth (VecCons _ v) (OSucc n) = unaryNth v n

-- GADT skew ordinals
data OSkew :: Unary -> * where

skewNth :: Vec a n -> OSkew n -> a
skewNth = undefined

-- GADT skew binary numbers
--data GSkew :: [Unary] -> * where
--  GSkew :: GSkew '[]
--  GSkewSucc :: GSkew '[d] ->