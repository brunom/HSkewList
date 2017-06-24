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
data ListRecord ls vs where
 LNil :: ListRecord '[] '[]
 LCons :: v -> ListRecord ls vs -> ListRecord (l ': ls) (v ': vs)
 
-- TODO primero con nuestras propias type funcs pero sin clases
 
-- todo partial types fail
-- get :: forall fl fv fs l fs1 v. (fs ~ ('(fl, fv) ': fs1), SingI (l :== fl), 'Just v ~ Lookup l fs) => ListRecord fs -> Sing l -> v
-- get (ListRecord (HCons v vs)) l =
    -- case (sing :: Sing (l :== fl)) of
    -- (STrue) -> v
    -- (SFalse) -> get (ListRecord vs :: ListRecord fs) l


type family Look l ls vs where
 Look l (l' ': ls) (v ': vs) = If (l :== l') v (Look l ls vs)

get ::
    SingI (Map ((:==$$) l) ls) =>
    ListRecord ls vs ->
    Sing l ->
    Look l ls vs
get = work sing where
    work ::
        Sing (Map ((:==$$) l) ls) ->
        ListRecord ls vs ->
        Sing l ->
        Look l ls vs
    work (SCons STrue  _) (LCons v _ ) = const v
    work (SCons SFalse m) (LCons _ vs) = work m vs

l1 = sing :: Sing "L1"
l2 = sing :: Sing "L2"
l3 = sing :: Sing "L3"

r :: ListRecord ["L1", "L2"] [Int, String]
r = LCons 42 (LCons "hi" LNil)
