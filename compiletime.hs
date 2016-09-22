{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

--import Data.HList.FakePrelude(HEq, hEq, HTrue, HFalse, HOr, hOr, Proxy, HSucc, HZero, HCond, hCond)
import Data.HList.Label4
import Data.HList.TypeEqGeneric1
import Data.HList.TypeCastGeneric1
import Data.HList

class HMakeMaybe b v m | b v -> m where
    hMakeMaybe :: b -> v -> m
instance HMakeMaybe HFalse v HNothing where
    hMakeMaybe b v = HNothing
instance HMakeMaybe HTrue v (HJust v) where
    hMakeMaybe b v = HJust v

data L1; l1 = undefined :: Proxy L1
data L2; l2 = undefined :: Proxy L2

class Set1 t t' | t -> t' where
  set1 :: t -> t'
instance Set1 (HNode e l HNil) (HNode (LVPair (Proxy L2) Int) l HNil) where
  set1 (HNode e l r) = HNode (l2 .=. 1) l r
instance Set1 (HNode e' l' r') (HNode e'' l' r'') =>
         Set1 (HNode e l (HNode e' l' r')) (HNode e l (HNode e'' l' r'')) where
  set1 (HNode e l r) = HNode e l (set1 r)

class HPlus a b c | a b -> c where
    hPlus :: a -> b -> c
instance HPlus (HJust a) HNothing (HJust a) where
    hPlus a b = a
instance HPlus HNothing (HJust b) (HJust b) where
    hPlus a b = b
instance HPlus HNothing HNothing HNothing where
    hPlus a b = HNothing

data  HEmpty           =  HEmpty
data  HNode  e  t  t'  =  HNode  e  t  t'
type  HLeaf  e         =  HNode e HEmpty HEmpty
hLeaf        e         =  HNode e HEmpty HEmpty

class HSkewHasField l ts v | l ts -> v where
    hSkewGet :: l -> ts -> v
instance HSkewHasField l HNil HNothing where
    hSkewGet _ _ = HNothing
instance HSkewHasField l HEmpty HNothing where
    hSkewGet _ _ = HNothing
instance
    (  HSkewHasField l t vt
    ,  HSkewHasField l ts vts
    ,  HPlus vt vts v) =>
       HSkewHasField l (HCons t ts) v where
    hSkewGet l (HCons t ts) =
        hSkewGet l t `hPlus` hSkewGet l ts
instance
    (  HSkewHasField l e et
    ,  HSkewHasField l t vt
    ,  HSkewHasField l t' vt'
    ,  HPlus et vt evt
    ,  HPlus evt vt' v) =>
       HSkewHasField l (HNode e t t') v where
    hSkewGet l (HNode e t t') =
        hSkewGet l e 
            `hPlus` hSkewGet l t 
               `hPlus` hSkewGet l t'
instance
    (  HEq l l' b
    ,  HMakeMaybe b v m) =>
       HSkewHasField l (LVPair l' v) m where
    hSkewGet l f =
        hMakeMaybe
            (hEq l (labelLVPair f))
            (valueLVPair f)

main = go (999999::Int) where
    go i = if i == 0 then return() else go (i - case hSkewGet l2 (make i) of HJust v -> v)

{-# NOINLINE make #-}
make i = list

tree =
  set1 $
  double_tree $
  double_tree $
  double_tree $
  double_tree $
  double_tree $
  double_tree $
  double_tree $
  HNil

double_tree t = HNode (l1 .=. ()) t t

list =
  l1 .=. () .*.
  l1 .=. () .*.
  l1 .=. () .*.
  l1 .=. () .*.
  l1 .=. () .*.
  l1 .=. () .*.
  l1 .=. () .*.
  l2 .=. 1 .*.
  HNil