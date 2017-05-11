{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}

module Data.Vinyl.CoreSkew where

import Data.Monoid
import Foreign.Ptr (castPtr, plusPtr)
import Foreign.Storable (Storable(..))
import Data.Vinyl.Functor
#if __GLASGOW_HASKELL__ < 710
import Control.Applicative hiding (Const(..))
#endif
import Data.Typeable (Proxy(..))
import Data.List (intercalate)
import Data.Vinyl.TypeLevel
import Unsafe.Coerce
foo = 42

-- | A record is parameterized by a universe @u@, an interpretation @f@ and a
-- list of rows @rs@.  The labels or indices of the record are given by
-- inhabitants of the kind @u@; the type of values at any label @r :: u@ is
-- given by its interpretation @f r :: *@.
data Rec :: (u -> *) -> [u] -> * where
  RNil :: Rec f '[]
  RCons :: !(Int) -> !(Tree f) -> !(Rec f rs) -> Rec f rs' -- int to snat

data Tree :: (u -> *) -> * where
  TLeaf :: !(f r) -> Tree f
  TNode :: !(f r) -> !(Tree f) -> !(Tree f) -> Tree f -- TODO balanced

data Decons f drs' where -- TODO make without
  DeconsJust :: (f dr) -> (Rec f drs) -> Decons f (dr ': drs)
  DeconsNothing :: Decons f '[]

decons :: Rec f rs' -> Decons f rs'
decons (RCons _ (TLeaf a) rs) = unsafeCoerce $ DeconsJust a rs
decons (RCons i (TNode a t t') rs) = unsafeCoerce $ DeconsJust a (unsafeCoerce ((RCons (i-1) t (RCons (i-1) t' rs)))) -- HACK
decons RNil = DeconsNothing

-- TODO Verify that the (:&) pattern has the sametrictness as the old ctor.
pattern (:&) :: () => (rs' ~ (r ': rs)) => f r -> Rec f rs -> Rec f rs'
pattern a :& b <- (decons -> DeconsJust a b) where
  --a :& (RCons i t (RCons i' t' rs)) | i == i' = unsafeCoerce (RCons (i+1) (TNode a t t') rs)
  a :& rs = RCons 0 (TLeaf a) rs

infixr 7 :&
infixr 5  <+>
infixl 8 <<$>>
infixl 8 <<*>>

-- | Two records may be pasted together.
rappend
  :: Rec f as
  -> Rec f bs
  -> Rec f (as ++ bs)
rappend RNil ys = ys
rappend (x :& xs) ys = x :& (xs `rappend` ys)

-- | A shorthand for 'rappend'.
(<+>)
  :: Rec f as
  -> Rec f bs
  -> Rec f (as ++ bs)
(<+>) = rappend

-- | 'Rec' @_ rs@ with labels in kind @u@ gives rise to a functor @Hask^u ->
-- Hask@; that is, a natural transformation between two interpretation functors
-- @f,g@ may be used to transport a value from 'Rec' @f rs@ to 'Rec' @g rs@.
rmap
  :: (forall x. f x -> g x)
  -> Rec f rs
  -> Rec g rs
rmap _ RNil = RNil
rmap η (x :& xs) = η x :& (η `rmap` xs)
{-# INLINE rmap #-}

-- | A shorthand for 'rmap'.
(<<$>>)
  :: (forall x. f x -> g x)
  -> Rec f rs
  -> Rec g rs
(<<$>>) = rmap
{-# INLINE (<<$>>) #-}

-- | An inverted shorthand for 'rmap'.
(<<&>>)
  :: Rec f rs
  -> (forall x. f x -> g x)
  -> Rec g rs
xs <<&>> f = rmap f xs
{-# INLINE (<<&>>) #-}

-- | A record of components @f r -> g r@ may be applied to a record of @f@ to
-- get a record of @g@.
rapply
  :: Rec (Lift (->) f g) rs
  -> Rec f rs
  -> Rec g rs
rapply RNil RNil = RNil
rapply (f :& fs) (x :& xs) = getLift f x :& (fs `rapply` xs)
{-# INLINE rapply #-}

-- | A shorthand for 'rapply'.
(<<*>>)
  :: Rec (Lift (->) f g) rs
  -> Rec f rs
  -> Rec g rs
(<<*>>) = rapply
{-# INLINE (<<*>>) #-}

-- | Given a section of some functor, records in that functor of any size are
-- inhabited.
class RecApplicative rs where
  rpure
    :: (forall x. f x)
    -> Rec f rs
instance RecApplicative '[] where
  rpure _ = RNil
  {-# INLINE rpure #-}
instance RecApplicative rs => RecApplicative (r ': rs) where
  rpure s = s :& rpure s
  {-# INLINE rpure #-}

-- | A record may be traversed with respect to its interpretation functor. This
-- can be used to yank (some or all) effects from the fields of the record to
-- the outside of the record.
rtraverse
  :: Applicative h
  => (forall x. f x -> h (g x))
  -> Rec f rs
  -> h (Rec g rs)
rtraverse _ RNil      = pure RNil
rtraverse f (x :& xs) = (:&) <$> f x <*> rtraverse f xs
{-# INLINABLE rtraverse #-}

-- | A record with uniform fields may be turned into a list.
recordToList
  :: Rec (Const a) rs
  -> [a]
recordToList RNil = []
recordToList (x :& xs) = getConst x : recordToList xs

-- | Wrap up a value with a capability given by its type
data Dict c a where
  Dict
    :: c a
    => a
    -> Dict c a

-- | Sometimes we may know something for /all/ fields of a record, but when
-- you expect to be able to /each/ of the fields, you are then out of luck.
-- Surely given @∀x:u.φ(x)@ we should be able to recover @x:u ⊢ φ(x)@! Sadly,
-- the constraint solver is not quite smart enough to realize this and we must
-- make it patently obvious by reifying the constraint pointwise with proof.
reifyConstraint
  :: RecAll f rs c
  => proxy c
  -> Rec f rs
  -> Rec (Dict c :. f) rs
reifyConstraint prx rec =
  case rec of
    RNil -> RNil
    (x :& xs) -> Compose (Dict x) :& reifyConstraint prx xs

-- | Records may be shown insofar as their points may be shown.
-- 'reifyConstraint' is used to great effect here.
instance RecAll f rs Show => Show (Rec f rs) where
  show xs =
    (\str -> "{" <> str <> "}")
      . intercalate ", "
      . recordToList
      . rmap (\(Compose (Dict x)) -> Const $ show x)
      $ reifyConstraint (Proxy :: Proxy Show) xs

instance Monoid (Rec f '[]) where
  mempty = RNil
  RNil `mappend` RNil = RNil

instance (Monoid (f r), Monoid (Rec f rs)) => Monoid (Rec f (r ': rs)) where
  mempty = mempty :& mempty
  (x :& xs) `mappend` (y :& ys) = (x <> y) :& (xs <> ys)

instance Eq (Rec f '[]) where
  _ == _ = True
instance (Eq (f r), Eq (Rec f rs)) => Eq (Rec f (r ': rs)) where
  (x :& xs) == (y :& ys) = (x == y) && (xs == ys)

instance Ord (Rec f '[]) where
  compare _ _ = EQ
instance (Ord (f r), Ord (Rec f rs)) => Ord (Rec f (r ': rs)) where
  compare (x :& xs) (y :& ys) = mappend (compare x y) (compare xs ys)

instance Storable (Rec f '[]) where
  sizeOf _    = 0
  alignment _ = 0
  peek _      = return RNil
  poke _ RNil = return ()

instance (Storable (f r), Storable (Rec f rs)) => Storable (Rec f (r ': rs)) where
  sizeOf _ = sizeOf (undefined :: f r) + sizeOf (undefined :: Rec f rs)
  {-# INLINABLE sizeOf #-}
  alignment _ =  alignment (undefined :: f r)
  {-# INLINABLE alignment #-}
  peek ptr = do !x <- peek (castPtr ptr)
                !xs <- peek (ptr `plusPtr` sizeOf (undefined :: f r))
                return $ x :& xs
  {-# INLINABLE peek #-}
  poke ptr (!x :& xs) = poke (castPtr ptr) x >> poke (ptr `plusPtr` sizeOf (undefined :: f r)) xs
  {-# INLINEABLE poke #-}
