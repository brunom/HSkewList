{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Skew (
module Data.HList,
module Data.HList.Label4,
module Data.HList.TypeEqGeneric1,
module Data.HList.TypeCastGeneric1,
HNil(..),
HCons(..)
) where

import Data.HList hiding (HNil, HCons, HasField', hLookupByLabel', hExtend')
import Data.HList.Label4
import Data.HList.TypeEqGeneric1
import Data.HList.TypeCastGeneric1

data HNil = HNil
data HCons e l1 l2 l3 = HCons e l1 l2 l3

instance (HList l1, HList l2, HList l3)
      => HList (HCons e l1 l2 l3)

instance (HLength l1 n1, HNat n1, HList l1, HList l2, HList l3)
      => HLength (HCons a l1 l2 l3) (HSucc n1)

class HExtend' l2 l3 l4 where
  hExtend' :: e1 -> l2 -> HCons e1 l2 l3 l4
instance HExtend' l2 l2 l2 where
  hExtend' e1 l2 = HCons e1 l2 l2 l2

instance HExtend e HNil (HCons e HNil HNil HNil) where
  hExtend e HNil = HCons e HNil HNil HNil
instance HExtend' (HCons e2 l3 l4 l5) l6 l7 => HExtend e1 (HCons e2 l3 l4 l5) (HCons e1 (HCons e2 l3 l4 l5) l6 l7) where
  hExtend e1 l2 = hExtend' e1 l2

{--instance HExtend e (HCons e l1 l2 HNil) (HCons e (HCons e l1 l2 HNil) (HCons e l1 l2 HNil) (HCons e l1 l2 HNil)) where
  hExtend e1 l = HCons e1 l l l
instance HExtend
         e1
         (HCons e2 l1 l2 (HCons e3 l3 l4 l5))
         (HCons
          e1
          (HCons e2 l1 l2 (HCons e3 l3 l4 l5))
          (HCons e2 l1 l2 (HCons e3 l3 l4 l5))
          (HCons e2 l1 l2 (HCons e3 l3 l4 l5))) where
  hExtend e l = HCons e l l l
--}

class HasFieldB l r b | l r -> b where
instance HasFieldB l HNil HFalse
instance (HasFieldB l t bt, HasFieldB l ts bts, HOr bt bts b)
    => HasFieldB l (HCons t ts ts2 ts3) b
instance HEq l l' b => HasFieldB l (LVPair l' v) b
hasField :: HasFieldB l r b => l -> r -> b
hasField = undefined

class HasField' l e be l1 b1 l2 b2 l3 b3 v where
  hLookupByLabel' :: l -> e -> be -> l1 -> b1 -> l2 -> b2 -> l3 -> b3 -> v
instance (HasField l l3 v) => HasField' l e be l1 b1 l2 b2 l3 HTrue v where
  hLookupByLabel' l e be l1 b1 l2 b2 l3 b3 = hLookupByLabel l l3
instance (HasField l l2 v) => HasField' l e be l1 b1 l2 HTrue l3 HFalse v where
  hLookupByLabel' l e be l1 b1 l2 b2 l3 b3 = hLookupByLabel l l2
instance (HasField l l1 v) => HasField' l e be l1 HTrue l2 HFalse l3 HFalse v where
  hLookupByLabel' l e be l1 b1 l2 b2 l3 b3 = hLookupByLabel l l1
instance HasField' l (LVPair l v) HTrue l1 HFalse l2 HFalse l3 HFalse v where
  hLookupByLabel' l e be l1 b1 l2 b2 l3 b3 = valueLVPair e

instance (HasFieldB l e be,
          HasFieldB l l1 b1,
          HasFieldB l l2 b2,
          HasFieldB l l3 b3,
          HasField' l e be l1 b1 l2 b2 l3 b3 v) =>
         HasField l (HCons e l1 l2 l3) v where
  hLookupByLabel l (HCons e l1 l2 l3) = 
    hLookupByLabel'
     l
     e (hasField l e)
     l1 (hasField l l1)
     l2 (hasField l l2)
     l3 (hasField l l3)
