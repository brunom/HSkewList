{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms           #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns              #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
--module Data.Vinyl.Core where

import Data.Monoid
import Foreign.Ptr (castPtr, plusPtr)
import Foreign.Storable (Storable(..))
import Data.Vinyl.Functor
#if __GLASGOW_HASKELL__ < 710
import Control.Applicative hiding (Const(..))
#endif
import Data.Typeable (Proxy(..))
import Data.List (intercalate)
--import Data.Vinyl.TypeLevel
import Unsafe.Coerce
import Data.Type.Equality

-- | Append for type-level lists.
type family (as :: [k]) ++ (bs :: [k]) :: [k] where
  '[] ++ bs = bs
  (a ': as) ++ bs = a ': (as ++ bs)

-- | A record is parameterized by a universe @u@, an interpretation @f@ and a
-- list of rows @rs@.  The labels or indices of the record are given by
-- inhabitants of the kind @u@; the type of values at any label @r :: u@ is
-- given by its interpretation @f r :: *@.
data Rec :: (u -> *) -> [u] -> * where
  RNil :: Rec f '[]
  RCons :: !(Int) -> !(Tree f rs') -> !(Rec f rs) -> Rec f (rs' ++ rs) -- int to snat

data Tree :: (u -> *) -> [u] -> * where
  TLeaf :: !(f r) -> Tree f '[r]
  TNode :: !(f r) -> !(Tree f rs') -> !(Tree f rs) -> Tree f (r ': (rs' ++ rs)) -- TODO balanced

data Decons f rs' where -- TODO make without
  DeconsJust :: (f r) -> (Rec f rs) -> Decons f (r ': rs)
  DeconsNothing :: Decons f '[]
decons :: Rec f rs' -> Decons f rs'
decons (RCons _ (TLeaf a) rs) = DeconsJust a rs
decons (RCons i (TNode a t t') rs) = DeconsJust a (unsafeCoerce ((RCons (i-1) t (RCons (i-1) t' rs)))) -- HACK
decons RNil = DeconsNothing

-- TODO Verify that the (:&) pattern has the sametrictness as the old ctor.
pattern (:&) :: () => (rs' ~ (r ': rs)) => f r -> Rec f rs -> Rec f rs'
pattern a :& b <- (decons -> DeconsJust a b) where
  a :& (RCons i t (RCons i' t' rs)) | i == i' = unsafeCoerce (RCons (i+1) (TNode a t t') rs)
  a :& rs = RCons 0 (TLeaf a) rs

infixr 7 :&
--infixr 5  <+>
--infixl 8 <<$>>
--infixl 8 <<*>>

-- | Two records may be pasted together.
rappend
  :: Rec f as
  -> Rec f bs
  -> Rec f (as ++ bs)
rappend RNil ys = ys
rappend (x :& xs) ys = x :& (xs `rappend` ys)
