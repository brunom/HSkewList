{-# LANGUAGE DataKinds, TypeOperators, GADTs, TypeFamilies, RecordWildCards, TemplateHaskell, FunctionalDependencies, TypeSynonymInstances, FlexibleInstances, UndecidableInstances, ExistentialQuantification, ScopedTypeVariables, PolyKinds, RankNTypes, FlexibleContexts, ConstraintKinds #-}

{-# OPTIONS_GHC -fprint-explicit-foralls -fprint-explicit-kinds #-}

module Nat where

import Data.Singletons.TH
import Data.Singletons.Prelude
import Data.Singletons.Prelude.Base

$(singletons [d|
 -- Unary naturals
 data Unary = Z | S Unary
   deriving (Show, Eq, Ord)
 |])


-- Singleton unary shared naturals
-- Each element has a (lazy) link to its successor,
-- so the second time a successor is needed the node is reused.
data GSUnary :: Unary -> * where
  GSZero :: GSUnary (S Z) -> GSUnary Z
  GSSucc :: {gspred :: GSUnary a, _gssucc :: GSUnary (S (S a))} -> GSUnary (S a)

-- define successor manually, since record syntax fails: https://ghc.haskell.org/trac/ghc/ticket/9273
gssucc :: GSUnary a -> GSUnary (S a)
gssucc (GSZero s) = s
gssucc (GSSucc p s) = s

-- smart constructor/entry point
gszero = GSZero (gsbuild gszero)
gsbuild :: GSUnary n -> GSUnary (S n)
gsbuild pred = node
  where node = GSSucc pred (gsbuild node)

$(singletons [d|

 newtype Seg = Seg [(Unary, Unary)]
   deriving (Show)

 -- Segmented binary naturals
 -- Instead of storing the list of bits for a binary number,
 -- compress each contiguous run of 0s or 1s to its length.
 -- The number zero has no runs.
 -- Other numbers have an even number of runs, so store a list of pairs.
 -- The first runs is the least significant zeroes of the number and may be empty.
 -- The other runs are always at least one digit long, so we store one less than the length.
 -- 1111000001b has 0 zeroes, 1 one, 5 zeroes, and 4 ones, so becomes 0043.

 -- 0b, the empty run list, becomes 1b, which has no trailing zeroes and only 1 one.
 segSucc (Seg [])                   = Seg [(Z, Z)]

 -- A binary number consisting of just (n+1) ones becomes (n+1) zeroes followed by a single one.
 segSucc (Seg [(Z,n)])              = Seg [(S n, Z)]

 -- But if the number continues after the initial run of ones, two cases arise.
 -- When a single zero precedes the second run of ones, the zero disappears and the runs are merged.
 -- For example, 111011 becomes 111100.
 segSucc (Seg ((Z,n):(Z,m):ns))     = Seg ((S n, S m):ns)

 -- When several zeroes precedes the second run of ones, the run of zeroes is shortened.
 -- For example, 1110011 becomes 1110100.
 segSucc (Seg ((Z,n):(S x, m):ns))  = Seg ((S n, Z):(x, m):ns)

 -- The number may also start with a single zero, which disappears, growing the following run of ones.
 -- For example, 11110 becomes 1111.
 segSucc (Seg ((S Z, n):ns))        = Seg ((Z, S n):ns)

 -- But if the initial run of zeroes is not singular, a new pair of runs is prepended.
 -- For example, 1100 becomes 1101.
 segSucc (Seg ((S (S n), m):ns))    = Seg ((Z, Z):(n, m):ns)

 segPred (Seg [(Z, Z)])             = Seg []
 segPred (Seg [(S n, Z)])           = Seg [(Z,n)]
 segPred (Seg ((S n, S m):ns))      = Seg ((Z,n):(Z,m):ns)
 segPred (Seg ((S n, Z):(x, m):ns)) = Seg ((Z,n):(S x, m):ns)
 segPred (Seg ((Z, S n):ns))        = Seg ((S Z, n):ns)
 segPred (Seg ((Z, Z):(n, m):ns))   = Seg ((S (S n), m):ns)
 |])

$(singletons [d|
 newtype Skew = Skew [Unary]
   deriving (Show)

 -- Skew binary naturals
 -- Mathematically, skew numbers are of the form Sum (dn*(2^n-1)) with dn in {0,1}
 -- but for the first nonzero d that may also be 2.
 -- So 20 = 1+1+3+15 = 2^1-1 + 2^1-1 + 2^2-1 + 2^4-1 = [1,1,2,4].
 -- We store the difference between adjacent nonzero digits.
 -- To make the transformation injective, we cons 1 before computing the differences.
 -- To make it surjective, since digits don't repeat, we substract 1 from all positions but the first two.
 skewSucc (Skew [])           = Skew [Z]
 skewSucc (Skew [d])          = Skew [Z,d]
 skewSucc (Skew (d:Z:ds))     = Skew (S d:ds)
 skewSucc (Skew (d1:S d2:ds)) = Skew (Z:d1:d2:ds)

 skewPred (Skew [Z])          = Skew []
 skewPred (Skew [Z,d])        = Skew [d]
 skewPred (Skew (Z:d1:d2:ds)) = Skew (d1:S d2:ds)
 skewPred (Skew (S d:ds))     = Skew (d:Z:ds)
 |])

$(promote [d|
 skew2nat :: Skew -> Unary
 skew2nat (Skew []) = Z
 skew2nat a = S $ skew2nat $ skewPred a
 |])

$(singletons [d|
 nat2skew :: Unary -> Skew
 nat2skew Z = Skew []
 nat2skew (S a) = skewSucc $ nat2skew a
 |])

sSkew2nat :: Sing m -> Sing (Skew2nat m)
sSkew2nat (SSkew SNil) = SZ
sSkew2nat (SSkew (SCons SZ SNil))          = SS SZ
sSkew2nat (SSkew (SCons SZ (SCons d SNil)))        = SS $ sSkew2nat (SSkew (SCons d SNil))
sSkew2nat (SSkew (SCons SZ (SCons d1 (SCons d2 ds)))) = SS $ sSkew2nat (SSkew (SCons d1 (SCons (SS d2) ds)))
sSkew2nat (SSkew (SCons (SS d) ds)) = SS $ sSkew2nat (SSkew (SCons d (SCons SZ ds)))

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

mySUpred :: Sing (S n) -> Sing n
mySUpred = sUpred

type family UUKompare (n::Unary) (m::Unary) :: Ordering where
  UUKompare Z Z = EQ
  UUKompare (S n) Z = GT
  UUKompare Z (S n) = LT
  UUKompare (S m) (S n) = UUKompare m n

class Less n m
instance Less Z (S a)
instance Less n m => Less (S n) (S m)

--data PVec a n m = Vec n m

--skewNth :: 

unaryNth :: Compare m n ~ LT => Vec a n -> SUnary m -> a
--unaryNth :: UUKompare m n ~ LT => Vec a n -> SUnary m -> a
--unaryNth :: Vec a n -> SUnary m -> a
--unaryNth :: Less m n => Vec a n -> SUnary m -> a
unaryNth (VecCons a _) SZ = a
unaryNth (VecCons _ v) (SS n) = unaryNth v n
--unaryNth (VecCons _ v) n = unaryNth v (sUpred n)
--unaryNth (VecCons _ v) n = unaryNth v (mySUpred n)

type family Kompare (n::Skew) (m::Unary) :: Ordering where
  Kompare ('Skew '[]) Z = EQ
  Kompare a Z = GT
  Kompare ('Skew '[]) (S n) = LT
  Kompare a (S n) = Kompare (SkewPred a) n

--skewNth :: Kompare m n ~ LT => Vec a n -> Sing (m::[Unary]) -> a
--skewNth :: Compare (Skew2nat m) n ~ LT => Vec a n -> Sing (m::[Unary]) -> a
skewNth :: Vec a n -> Sing (m::Skew) -> a
skewNth (VecCons a _) (SSkew SNil) = a
skewNth (VecCons _ v) m = skewNth v (sSkewPred m)
-- skewNth (VecCons _ v) (SCons SZ SNil)          = skewNth v SNil
-- skewNth (VecCons _ v) (SCons SZ (SCons d SNil))        = skewNth v (SCons d SNil)
-- skewNth (VecCons _ v) (SCons SZ (SCons d1 (SCons d2 ds))) = skewNth v (SCons d1 (SCons (SS d2) ds))
-- skewNth (VecCons _ v) (SCons (SS d) ds) = skewNth v (SCons d (SCons SZ ds))
