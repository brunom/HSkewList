{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

-- skew
{-# LANGUAGE PatternSynonyms           #-}
{-# LANGUAGE ViewPatterns              #-}



{-# LANGUAGE TemplateHaskell              #-}
-- {-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
--module Data.Vinyl.Core where

import Data.Monoid
import Foreign.Ptr (castPtr, plusPtr)
import Foreign.Storable (Storable(..))
--import Data.Vinyl.Functor
#if __GLASGOW_HASKELL__ < 710
import Control.Applicative hiding (Const(..))
#endif
import Data.Typeable (Proxy(..))
import Data.List (intercalate)
--import Data.Vinyl.TypeLevel


-- skew
import Unsafe.Coerce


--import Data.Vinyl
--import Data.Vinyl.Functor
import Control.Applicative
import Control.Lens hiding (Identity)
import Control.Lens.TH
import Data.Char
--import Test.DocTest
import Data.Singletons.TH
import Data.Maybe


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
--  RCons :: !(Int) -> !(Tree f rs') -> !(Rec f rs) -> Rec f (rs' ++ rs) -- int to snat
  RCons :: !(Int) -> !(Tree f) -> !(Rec f rs) -> Rec f rs' -- int to snat

data Tree :: (u -> *) -> * where
  TLeaf :: !(f r) -> Tree f
  TNode :: !(f r) -> !(Tree f) -> !(Tree f) -> Tree f -- TODO balanced
--data Tree :: (u -> *) -> [u] -> * where
--  TLeaf :: !(f r) -> Tree f '[r]
--  TNode :: !(f r) -> !(Tree f rs') -> !(Tree f rs) -> Tree f (r ': (rs' ++ rs)) -- TODO balanced

data Decons f drs' where -- TODO make without
  DeconsJust :: (f dr) -> (Rec f drs) -> Decons f (dr ': drs)
  DeconsNothing :: Decons f '[]

decons :: Rec f rs' -> Decons f rs'
decons (RCons _ (TLeaf a) rs) = unsafeCoerce $ DeconsJust a rs
--decons (RCons _ (TLeaf a) rs) = DeconsJust a rs
decons (RCons i (TNode a t t') rs) = unsafeCoerce $ DeconsJust a (unsafeCoerce ((RCons (i-1) t (RCons (i-1) t' rs)))) -- HACK
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




data Fields = Name | Age | Sleeping | Master deriving Show

type LifeForm = [Name, Age, Sleeping]

type family ElF (f :: Fields) :: * where
  ElF Name = String
  ElF Age = Int
  ElF Sleeping = Bool
  ElF Master = Rec Attr LifeForm
newtype Attr f = Attr { _unAttr :: ElF f }
makeLenses ''Attr
genSingletons [ ''Fields ]
instance Show (Attr Name) where show (Attr x) = "name: " ++ show x
instance Show (Attr Age) where show (Attr x) = "age: " ++ show x
instance Show (Attr Sleeping) where show (Attr x) = "sleeping: " ++ show x
--instance Show (Attr Master) where show (Attr x) = "master: " ++ show x

(=::) :: sing f -> ElF f -> Attr f
_ =:: x = Attr x

jon = (SName =:: "jon")
      :& (SAge =:: 23)
      :& (SSleeping =:: False)
      :& RNil

tucker = (SName =:: "tucker")
         :& (SAge =:: 9)
         :& (SSleeping =:: True)
         :& (SMaster =:: jon)
         :& RNil

wakeUp = rput $ SSleeping =:: False