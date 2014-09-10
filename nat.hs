{-# LANGUAGE DataKinds, TypeOperators, GADTs, TypeFamilies, RecordWildCards, TemplateHaskell, FunctionalDependencies, TypeSynonymInstances, FlexibleInstances, UndecidableInstances, ExistentialQuantification, ScopedTypeVariables, PolyKinds, RankNTypes, FlexibleContexts, ConstraintKinds #-}

import Data.Singletons.TH
import Data.Singletons.Prelude
import Data.Singletons.Prelude.Base

$(singletons [d|
 -- Unary naturals
 data Unary = Z | S Unary
   deriving (Show, Eq, Ord)
 |])




-- Singleton unary shared naturals
data GSUnary :: Unary -> * where
  GSZero :: GSUnary (S Z) -> GSUnary Z
  GSSucc :: GSUnary a -> GSUnary (S (S a)) -> GSUnary (S a)

gssucc :: GSUnary a -> GSUnary (S a)
gssucc (GSZero s) = s
gssucc (GSSucc p s) = s

gszero = GSZero (gsbuild gszero)
gsbuild :: GSUnary n -> GSUnary (S n)
gsbuild pred = node
  where node = GSSucc pred (gsbuild node)

-- TODO explicar la invariante, etc

$(singletons [d|
 -- Segmented binary naturals
 segSucc []                   = [(Z, Z)]
 segSucc [(Z,n)]              = [(S n, Z)]
 segSucc ((Z,n):(Z,m):ns)     = ((S n, S m):ns)
 segSucc ((Z,n):(S x, m):ns)  = ((S n, Z):(x, m):ns)
 segSucc ((S Z, n):ns)        = ((Z, S n):ns)
 segSucc ((S (S n), m):ns)    = ((Z, Z):(n, m):ns)

 segPred [(Z, Z)]             = []
 segPred [(S n, Z)]           = [(Z,n)]
 segPred ((S n, S m):ns)      = ((Z,n):(Z,m):ns)
 segPred ((S n, Z):(x, m):ns) = ((Z,n):(S x, m):ns)
 segPred ((Z, S n):ns)        = ((S Z, n):ns)
 segPred ((Z, Z):(n, m):ns)   = ((S (S n), m):ns)
 |])

$(singletons [d|
 -- Skew binary naturals
 skewSucc []           = [Z]
 skewSucc [d]          = [Z,d]
 skewSucc (d:Z:ds)     = (S d:ds)
 skewSucc (d1:S d2:ds) = (Z:d1:d2:ds)

 skewPred [Z]          = []
 skewPred [Z,d]        = [d]
 skewPred (Z:d1:d2:ds) = (d1:S d2:ds)
 skewPred (S d:ds)     = (d:Z:ds)
 |])

$(promote [d|
 skew2nat [] = Z
 skew2nat a = S $ skew2nat $ skewPred a
 |])

data Tree (n::Unary) a where
  TreeLeaf :: a -> Tree Z a
  TreeNode :: a -> Tree n a -> Tree n a -> Tree (S n) a

data SkewList (n::[Unary]) (h::Unary) a where
  SkewNil :: SkewList '[] h a
  SkewCons :: Tree h a -> SkewList n (S h) a -> SkewList (h ': n) h a

-- Length indexed lists
data Vec :: * -> Unary -> * where
  VecNil :: Vec a Z
  VecCons :: a -> Vec a n -> Vec a (S n)

$(singletons [d|
 usucc n = S n
 upred (S n) = n
 |])

unaryNth :: Compare m n ~ LT => Vec a n -> SUnary m -> a
unaryNth (VecCons a _) SZ = a
unaryNth (VecCons _ v) (SS n) = unaryNth v n
--unaryNth (VecCons _ v) n = unaryNth v (sUpred n)

type family Kompare (n::[Unary]) (m::Unary) :: Ordering where
  Kompare '[] Z = EQ
  Kompare a Z = GT
  Kompare '[] (S n) = LT
  Kompare a (S n) = Kompare (SkewPred a) n

--skewNth :: Kompare m n ~ LT => Vec a n -> Sing (m::[Unary]) -> a
skewNth :: Compare (Skew2nat m) n ~ LT => Vec a n -> Sing (m::[Unary]) -> a
skewNth (VecCons a _) SNil = a
--skewNth (VecCons _ v) m = skewNth v (sSkewPred m)
skewNth (VecCons _ v) (SCons SZ SNil)          = skewNth v SNil
skewNth (VecCons _ v) (SCons SZ (SCons d SNil))        = skewNth v (SCons d SNil)
skewNth (VecCons _ v) (SCons SZ (SCons d1 (SCons d2 ds))) = skewNth v (SCons d1 (SCons (SS d2) ds))
skewNth (VecCons _ v) (SCons (SS d) ds) = skewNth v (SCons d (SCons SZ ds))

