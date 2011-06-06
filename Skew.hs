{-#  LANGUAGE EmptyDataDecls  #-}
{-#  LANGUAGE FlexibleContexts  #-}
{-#  LANGUAGE TypeOperators  #-}
{-#  LANGUAGE MultiParamTypeClasses  #-}
{-#  LANGUAGE FunctionalDependencies  #-}
{-#  LANGUAGE FlexibleInstances  #-}
{-#  LANGUAGE UndecidableInstances  #-}

module Main where

import Data.HList.FakePrelude(HEq, hEq, HTrue, HFalse, HOr, hOr, Proxy, proxy, HSucc, HZero, HCond, hCond)
import Data.HList.Label4
import Data.HList.TypeEqGeneric1
import Data.HList.TypeCastGeneric1

infixr 2 .*.
infixr 4 .=.

data HNothing  = HNothing
data HJust e   = HJust e

class HMakeMaybe b v m | b v -> m where
    hMakeMaybe :: b -> v -> m
instance HMakeMaybe HFalse v HNothing where
    hMakeMaybe b v = HNothing
instance HMakeMaybe HTrue v (HJust v) where
    hMakeMaybe b v = HJust v

class HPlus a b c | a b -> c where
    hPlus :: a -> b -> c
instance HPlus (HJust a) b (HJust a) where
    hPlus a b = a
instance HPlus HNothing (HJust b) (HJust b) where
    hPlus a b = b
instance HPlus HNothing HNothing HNothing where
    hPlus a b = HNothing

data HNil       = HNil
data HCons e l  = HCons e l

class HExtend e l l' | e l -> l', l' -> e l
    where (.*.) :: e -> l -> l'



foo :: HExtend e l l' => l' -> (e, l)
foo = undefined

bar = foo (SkewRecord (hLeaf (() .=. ())))




newtype LVPair l v  =   LVPair { valueLVPair :: v }
(.=.)               ::  l -> v -> LVPair l v
_  .=.  v           =   LVPair v

labelLVPair :: LVPair l v -> l
labelLVPair = undefined

data  HEmpty           =  HEmpty
data  HNode  e  t  t'  =  HNode  e  t  t'
type  HLeaf  e         =  HNode e HEmpty HEmpty
hLeaf        e         =  HNode e HEmpty HEmpty

newtype SkewRecord r = SkewRecord r
emptySkewRecord :: SkewRecord HNil
emptySkewRecord = SkewRecord HNil

instance
    (HSkewExtend (LVPair l v) ts ts') =>
    -- HSkewHasField l ts HNothing) =>
    HExtend
        (LVPair l v)
        (SkewRecord ts)
        (SkewRecord ts') where
    e .*. SkewRecord ts =
        SkewRecord (hSkewExtend e ts)

class HComplete t h | t -> h
instance HComplete HEmpty HZero
instance
    (HComplete t h, HComplete t' h) =>
    HComplete (HNode e t t') (HSucc h)

class HSkewCarry l b | l -> b
instance HSkewCarry HNil HFalse
instance HSkewCarry (HCons t HNil) HFalse
instance
    (HComplete t h
    ,HComplete t' h'
    ,HEq h h' b)
    => HSkewCarry (HCons t (HCons t' ts)) b
hSkewCarry :: HSkewCarry l b => l -> b
hSkewCarry = undefined

class HSkewExtend e l l' | e l -> l'
    where hSkewExtend :: e -> l -> l'
instance
    (HSkewCarry l b
    ,HSkewExtend' b e l l') =>
    HSkewExtend e l l' where
    hSkewExtend e l = hSkewExtend' (hSkewCarry l) e l

class HSkewExtend' b e l l' | b e l -> l' where
    hSkewExtend' :: b -> e -> l -> l'
instance
    HSkewExtend'
        HFalse
        e
        l
        (HCons (HLeaf e) l) where
    hSkewExtend' _ e l = HCons (hLeaf e) l
instance
    HSkewExtend'
        HTrue
        e
        (HCons t (HCons t' l))
        (HCons (HNode e t t') l) where
    hSkewExtend' _ e (HCons t (HCons t' l)) =
        (HCons (HNode e t t') l)
