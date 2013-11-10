{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

import GHC.TypeLits

data HZero = HZero
data HSucc n = HSucc n

type family HSum a b
type instance HSum HZero b = b
type instance HSum (HSucc a) b = HSucc (HSum a b)

data HList (a :: [*]) where
  HNil :: HList '[]
  HCons :: a -> HList as -> HList (a ': as)

data Tree a
  = Empty
  | Node a (Tree a) (Tree a)

data HTree :: Tree * -> * -> * where
  HEmpty :: HTree Empty HZero
  HNode :: a -> HTree b h -> HTree c h -> HTree (Node a b c) (HSucc h)

data HSkewList :: [Tree *] -> * -> * where
  HSkewNil :: HSkewList '[] h
  HSkewCons :: HTree a h -> dh -> HSkewList as (HSum h dh) -> HSkewList (a ': as) h

--f :: HSkewList as 0 -> HSkewList (Empty ': as) 0
--f a = HSkewCons HEmpty a

work1 = HSkewCons HEmpty HZero $ HSkewNil
work2 = HSkewCons HEmpty HZero $ work1
--work3 = HSkewCons HEmpty $ HSkewCons (HNode 3 HEmpty HEmpty) $ HSkewNil
--fail4 =  HSkewCons HEmpty $ HSkewCons HEmpty $ HSkewCons HEmpty $ HSkewNil 

main = return ()