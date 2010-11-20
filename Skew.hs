{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Skew where
import Data.HList.FakePrelude

data HNil
    = HNil
data HCons e l
    = HCons e l
newtype HLeaf e
    = HLeaf e
data HNode e t t'
    = HNode e t t'

newtype LVPair l v = LVPair { valueLVPair :: v } deriving Eq
labelLVPair :: LVPair l v -> l
labelLVPair = undefined
newLVPair :: l -> v -> LVPair l v
newLVPair _ = LVPair

class HBalanced t h | t -> h
instance HBalanced (HLeaf e) HZero
instance (HBalanced t h, HBalanced t' h) => HBalanced (HNode e t t') (HSucc h)

hHeight :: HBalanced t h => t -> h
hHeight = undefined

infixr 2 .*.
-- class HExtend e ts ets | e ts -> ets, ets -> e ts where
class HExtend e ts ets | e ts -> ets where
    (.*.) :: e -> ts -> ets
instance HExtend e HNil (HCons (HLeaf e) HNil) where
    e .*. ts = (HCons (HLeaf e) ts)
instance HExtend e (HCons t HNil) (HCons (HLeaf e) (HCons t HNil)) where
    e .*. ts = (HCons (HLeaf e) ts)
instance
    (HBalanced t h
    ,HBalanced t' h'
    ,HEq h h' b
    ,HExtend' b e t t' ts ett'ts)
    => HExtend e (HCons t (HCons t' ts)) ett'ts where
    e .*. tt'ts@(HCons t (HCons t' ts)) =
        hExtend' (hEq (hHeight t) (hHeight t')) e tt'ts

class HExtend' b e t t' ts ett'ts | b e t t' ts -> ett'ts where
    hExtend' :: b -> e -> (HCons t (HCons t' ts)) -> ett'ts
instance HExtend' HTrue e t t' ts (HCons (HNode e t t') ts) where
    hExtend' _ e (HCons t (HCons t' ts)) = (HCons (HNode e t t') ts)
instance HExtend' HFalse e t t' ts (HCons (HLeaf e) (HCons t (HCons t' ts))) where
    hExtend' _ e tt'ts = (HCons (HLeaf e) tt'ts)

infixr 4 .=.
(.=.) :: l -> v -> LVPair l v
l .=. v = newLVPair l v

infixr 9 .!.
(.!.) :: (HasField l r v) => r -> l -> v
r .!. l =  hLookupByLabel l r

infixr 9 #
(#) :: (HasField l r v) => r -> l -> v
m # field = (m .!. field)

class HasField l ts v | l ts -> v where
    hLookupByLabel:: l -> ts -> v
instance
   (HasFieldB l t bt
   ,HasFieldB l ts bts
   ,HasField_HCons bt bts l t ts v) =>
    HasField l (HCons t ts) v where
    hLookupByLabel l (HCons t ts) =
        hLookupByLabel_HCons (hasField l t) (hasField l ts) l t ts
instance HasField l (HLeaf (LVPair l v)) v where
    hLookupByLabel l (HLeaf (LVPair v)) = v
instance
    (HEq l l' bl'
    ,HasFieldB l t bt
    ,HasFieldB l t' bt'
    ,HasField_HNode bl' bt bt' l v' t t' v)
    => HasField l (HNode (LVPair l' v') t t') v where
    hLookupByLabel l (HNode f@(LVPair v') t t') =
        hLookupByLabel_HNode
        (hEq l (labelLVPair f))
        (hasField l t)
        (hasField l t')
        l
        v'
        t
        t'

class HasFieldB l r b | l r -> b where
instance HasFieldB l HNil HFalse
instance (HasFieldB l t bt, HasFieldB l ts bts, HOr bt bts b)
    => HasFieldB l (HCons t ts) b
instance HEq l l' b => HasFieldB l (HLeaf (LVPair l' v)) b
instance
    (HEq l l' bl
    ,HasFieldB l l1 b1
    ,HasFieldB l l2 b2
    ,HOr bl b1 bl1
    ,HOr bl1 b2 bl12)
    => HasFieldB l (HNode (LVPair l' v) l1 l2) bl12
hasField :: HasFieldB l r b => l -> r -> b
hasField = undefined


class HasField_HCons bt bts l t ts v where
    hLookupByLabel_HCons :: bt -> bts -> l -> t -> ts -> v
instance
    HasField l t v =>
    HasField_HCons HTrue bts l t ts v where
    hLookupByLabel_HCons _ _ l t ts = hLookupByLabel l t
instance HasField l ts v => HasField_HCons HFalse HTrue l t ts v where
    hLookupByLabel_HCons _ _ l t ts = hLookupByLabel l ts

class HasField_HNode be bt bt' l e t t' v where
    hLookupByLabel_HNode :: be -> bt -> bt' -> l -> e -> t -> t' -> v
instance HasField_HNode HTrue bt bt' l (LVPair l v) t t' v where
    hLookupByLabel_HNode _ _ _ l e t t' = valueLVPair e
instance HasField l t v => HasField_HNode HFalse HTrue bt' l e t t' v where
    hLookupByLabel_HNode _ _ _ l e t t' = hLookupByLabel l t
instance HasField l t' v => HasField_HNode HFalse HFalse HTrue l e t t' v where
    hLookupByLabel_HNode _ _ _ l e t t' = hLookupByLabel l t'
