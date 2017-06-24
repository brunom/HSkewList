{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE PartialTypeSignatures  #-}
{-# LANGUAGE TypeFamilyDependencies  #-}
{-# OPTIONS_GHC -Wunticked-promoted-constructors #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-} -- HACK


import Data.Kind
import Data.Singletons
import Data.Singletons.TH
import Data.Singletons.TypeLits
import Data.Singletons.Prelude
import Data.Singletons.Prelude.List
import Data.Singletons.Prelude.Maybe

data HList (l:: [Type]) where
   HNil  :: HList '[]
   HCons :: e -> HList l -> HList (e ': l)
infixr 2 `HCons`
newtype ListRecord fs = ListRecord (HList (Map SndSym0 fs))

-- todo partial types fail
-- get :: forall fl fv fs l fs1 v. (fs ~ ('(fl, fv) ': fs1), SingI (l :== fl), 'Just v ~ Lookup l fs) => ListRecord fs -> Sing l -> v
-- get (ListRecord (HCons v vs)) l =
    -- case (sing :: Sing (l :== fl)) of
    -- (STrue) -> v
    -- (SFalse) -> get (ListRecord vs :: ListRecord fs) l

-- get ::
    -- forall m fs v l. (
    -- m ~ Map ((:==$$) l) (Map FstSym0 fs),
    -- SingI m,
    -- 'Just v ~ Lookup l fs) =>
    -- ListRecord fs ->
    -- Sing l ->
    -- v
-- get (ListRecord (HCons vv vs)) l =
    -- case (sing :: Sing m) of
    -- (SCons STrue _) -> vv

l1 = sing :: Sing "L1"
l2 = sing :: Sing "L2"
l3 = sing :: Sing "L3"

r :: ListRecord '[ '("L1", Int), '("L2", String)]
r = ListRecord (HCons 42 (HCons "hi" HNil))
