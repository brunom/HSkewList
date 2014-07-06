{-# LANGUAGE DataKinds, TypeOperators, GADTs, TypeFamilies, RecordWildCards, TemplateHaskell, FunctionalDependencies, TypeSynonymInstances, FlexibleInstances, UndecidableInstances, ExistentialQuantification, ScopedTypeVariables, PolyKinds, RankNTypes, FlexibleContexts, ConstraintKinds #-}

import Data.Singletons.TH
import Data.Singletons.Prelude
import Data.Singletons.Prelude.Base
import Data.Singletons.TypeLits
import GHC.TypeLits


-- Unary naturals
data Unary = Z | S Unary
  deriving Show

 -- Singleton unary shared naturals
data GSUnary :: Nat -> * where
  GSZero :: GSUnary 1 -> GSUnary 0
  GSSucc :: GSUnary a -> GSUnary (a+2) -> GSUnary (a+1)

gssucc :: GSUnary a -> GSUnary (a+1)
gssucc (GSZero s) = s
--gssucc (GSSucc p s) = s

-- gszero = GSZero (gsbuild gszero)
-- gsbuild :: GSUnary n -> GSUnary (n+1)
-- gsbuild pred = node
--   where node = GSSucc pred (gsbuild node)

-- Template Haskell doesn't recognize n+k patterns,
-- ruins the symmetry between pred and succ,
-- and forces the order of clauses.
$(promote [d|
 -- Segmented binary naturals
 segSucc []               = ((0,0):[])
 segSucc ((0,n):[])       = ((n+1,0):[])
 segSucc ((0,n):(0,m):ns) = ((n+1,m+1):ns)
 segSucc ((0,n):(x,m):ns) = ((n+1,0):(x-1,m):ns)
 segSucc ((1,n):ns)       = ((0,n+1):ns)
 segSucc ((n, m):ns)      = ((0,0):(n-1,m):ns)

 segPred ((0,0):[])       = []
 segPred ((0,0):(n,m):ns) = ((n+2,m):ns)
 segPred ((0,n):ns)       = ((1,n-1):ns)
 segPred ((n,0):[])       = ((0,n-1):[])
 segPred ((n,0):(x,m):ns) = ((0,n-1):(x+1, m):ns)
 segPred ((n,m):ns)       = ((0,n-1):(0,m-1):ns)

 -- Skew binary naturals
 skewSucc []           = [0]
 skewSucc [d]          = [0,d]
 skewSucc (d:0:ds)     = (d+1:ds)
 skewSucc (d1:d2:ds)   = (0:d1:d2-1:ds)

 skewPred [0]          = []
 skewPred [0,d]        = [d]
 skewPred (0:d1:d2:ds) = (d1:d2+1:ds)
 skewPred (d:ds)       = (d-1:0:ds)
 |])

data SNat (n::Unary) where
  SNE :: SNat Z 
  SNC :: NatFun n' n -> Unary -> SNat n' -> SNat n

instance Show (SNat a) where
 show SNE = "SNE"
 show (SNC f u r) = "SNC ("++ show f ++ "," ++ show u ++ "," ++ show r ++ ")"
   
data NatFun (n::Unary) (n'::Unary) where
  FS :: NatFun n (S n)
  FC :: NatFun n' n'' -> NatFun n n' -> NatFun n n''  
  
instance Show (NatFun n n') where   
  show FS = "FS"
  show (FC x y) = "FC(" ++ show x  ++ "|"++ show y  ++ ")"
  
sNSucc :: SNat n -> SNat (S n)
sNSucc SNE  = SNC FS Z SNE
sNSucc (SNC f d SNE) = SNC FS Z (SNC f d SNE)
sNSucc (SNC f d (SNC f2 Z ds)) = (SNC (FC FS (FC f f2)) (S d) ds) 
sNSucc (SNC f d1 (SNC f1 (S d2) ds)) = (SNC FS Z (SNC f d1 (SNC f1 d2 ds)))


sNPred :: SNat (S n) -> SNat n
sNPred (SNC FS Z SNE) = SNE  
sNPred (SNC FS Z (SNC f1 d SNE)) = (SNC f1 d SNE)
sNPred (SNC (FC FS (FC f f2)) (S d) ds) = (SNC f d (SNC f2 Z ds))  
sNPred (SNC FS Z (SNC f d1 (SNC f1 d2 ds))) = (SNC f d1 (SNC f1 (S d2) ds))

data PPF (n :: Nat) where
  PPFS :: PPF 1
  PPFC :: PPF n -> PPF (n + n + 1)

data PPNat (n :: Nat) where
  PPNil :: PPNat 0
  PPCons :: PPF n -> PPNat n' -> PPNat (n + n')

ppPred :: PPNat (1+n) -> PPNat n
ppPred (PPCons PPFS PPNil) = PPNil

cero = SNE 
uno = sNSucc cero

dos = sNSucc uno
tre = sNSucc dos
cua = sNSucc tre
cin = sNSucc cua
sei = sNSucc cin


-- -- Length indexed lists
-- data Vec :: * -> Unary -> * where
--   VecNil :: Vec a Z
--   VecCons :: a -> Vec a n -> Vec a (S n)

-- -- GADT unary ordinals
-- data OUnary :: Unary -> * where
--   OZero :: OUnary (S n)
--   OSucc :: OUnary n -> OUnary (S n)

-- unaryNth :: Vec a n -> OUnary n -> a
-- unaryNth (VecCons a _) OZero = a
-- unaryNth (VecCons _ v) (OSucc n) = unaryNth v n

-- -- GADT skew ordinals
-- data OSkew :: Unary -> * where

-- skewNth :: Vec a n -> OSkew n -> a
-- skewNth = undefined

-- -- GADT skew binary numbers
-- --data GSkew :: [Unary] -> * where
-- --  GSkew :: GSkew '[]
-- --  GSkewSucc :: GSkew '[d] ->
