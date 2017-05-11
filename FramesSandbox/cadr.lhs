> {-# LANGUAGE DataKinds, PolyKinds, GADTs, TypeFamilies,
>              LambdaCase #-}
> {-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
> 
> data Nat = Zero | Succ Nat

> data Vec :: * -> Nat -> * where
>   VNil  :: Vec a Zero
>   VCons :: a -> Vec a n -> Vec a (Succ n)
>
> safeHead :: Vec a (Succ n) -> a
> safeHead (VCons h _) = h
>
> safeTail :: Vec a (Succ n) -> Vec a n
> safeTail (VCons _ t) = t

> boolEq :: Nat -> Nat -> Bool
> boolEq Zero     Zero     = True
> boolEq (Succ a) (Succ b) = boolEq a b
> boolEq _        _        = False

> type family BoolEq (a :: Nat) (b :: Nat) :: Bool
> type instance BoolEq Zero Zero = True
> type instance BoolEq (Succ a) (Succ b) = BoolEq a b
> type instance BoolEq Zero (Succ x) = False
> type instance BoolEq (Succ x) Zero = False

>-- cadr :: Vec a n -> a
> cadr v = safeHead (safeTail v)