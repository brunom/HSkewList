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
data ListRecord fs where
 LNil :: ListRecord '[]
 LCons :: v -> ListRecord fs -> ListRecord ('(l,v) ': fs)
 
-- TODO primero con nuestras propias type funcs pero sin clases
 
-- todo partial types fail

get ::
    SingI (Map ((:==$$) l) (Map FstSym0 fs)) =>
    ListRecord fs ->
    Sing l ->
    FromJust (Lookup l fs)
get = work sing where
    work ::
        Sing (Map ((:==$$) l) (Map FstSym0 fs)) ->
        ListRecord fs ->
        Sing l ->
        FromJust (Lookup l fs)
    work (SCons STrue  _) (LCons v _ ) l = v
    work (SCons SFalse m) (LCons _ vs) l = work m vs l

l1 = sing :: Sing "L1"
l2 = sing :: Sing "L2"
l3 = sing :: Sing "L3"

r :: ListRecord ['("L1", Int), '("L2", String)]
r = LCons 42 (LCons "hi" LNil)
