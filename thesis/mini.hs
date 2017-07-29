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
{-# LANGUAGE LambdaCase  #-}
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
import GHC.TypeLits

data Field l v where
 Field:: SingI l => { value :: v } -> Field l v
(.=.)               ::  Sing l -> v -> Field l v
l .=.  v           = case singInstance l of SingInstance -> Field v
label  :: Field l v -> Sing l
label (Field v) = sing

data HList (l:: [Type]) where
   HNil  :: HList '[]
   HCons :: e -> HList l -> HList (e ': l)
infixr 2 `HCons`

data ListRecord fs where
 LNil :: ListRecord '[]
 LCons :: Field l v -> ListRecord fs -> ListRecord ('(l,v) ': fs)
infixr 2 `LCons`

-- todo partial types fail

type family Lookup2 l fs where
    Lookup2 l ((Field l' v) : fs) = If (l :== l') v (Lookup2 l fs)

-- No deberíamos necesitar SEq, porque si no se puede comparar tampoco se va a poder evaluar Lookup
get :: forall k (l::k) fs.
    SEq k =>
    ListRecord fs ->
    Sing l ->
    FromJust (Lookup l fs)
get (LCons f fs) l = case (l %:== (label f)) of -- careful with the order of params to eq
    STrue -> value f
    SFalse -> get fs l

-- data Tree h where
    -- Empty :: Tree 0
    -- Node :: e -> Tree h -> Tree h -> Tree (1 + h)
-- type Leaf e = 'Node e 'Empty 'Empty
-- data HTree h (t :: Tree h) where
    -- HEmpty :: HTree 0 'Empty
    -- HNode :: e -> HTree h t1 -> HTree h t2 -> HTree (1 + h) ('Node e t1 t2) 
-- type HLeaf e = 'HNode e 'HEmpty 'HEmpty

-- TODO classes for skew, list and array records

$(singletons [d|
    data Tree e
        =  Empty
        |  Node e (Tree e) (Tree e)
    |])

$(singletons [d|
    height :: Tree e -> Nat
    height Empty = 0
    height (Node _ _ t) = 1 + height t
    |])
type Leaf e = 'Node e 'Empty 'Empty
data HTree t where
    HEmpty :: HTree 'Empty
    HNode :: e -> HTree t1 -> HTree t2 -> HTree ('Node e t1 t2) 

data TreeList ts where
 TLNil :: TreeList '[]
 TLCons :: HTree t -> TreeList ts -> TreeList (t ': ts)
infixr 2 `TLCons`

newtype SkewRecord fs = SkewRecord (TreeList (Skew fs))

skewNil :: SkewRecord '[]
skewNil = SkewRecord TLNil

type family Skew fs where
    Skew '[] = '[]
    Skew ( '(l, v) ': fs) = Skew2 v (Skew fs)

type family Skew2 v ts where
    Skew2 v '[] = '[ Leaf v ]
    Skew2 v '[a] = '[ Leaf v, a]
    Skew2 v (a ': b ': ts) = If (Height a :== Height b)
        ( 'Node v a b ': ts)
        ( Leaf v ': a ': b ': ts)

skewCons :: forall l v fs s. (s ~ (Map HeightSym0 (Skew fs)), SingI s) => Field l v -> SkewRecord fs -> SkewRecord ('(l, v) : fs)
skewCons (Field v) = \case
    (SkewRecord TLNil) -> SkewRecord $ HNode v HEmpty HEmpty `TLCons` TLNil
    (SkewRecord (a `TLCons` TLNil)) -> SkewRecord (HNode v HEmpty HEmpty `TLCons` a `TLCons` TLNil)
    (SkewRecord (va `TLCons` vb `TLCons` vs)) -> case sing :: Sing s of
        (ha `SCons` (hb `SCons` _)) -> case ha %:== hb of
            STrue -> SkewRecord (HNode v va vb `TLCons` vs)
            SFalse -> SkewRecord (HNode v HEmpty HEmpty `TLCons` va `TLCons` vb `TLCons` vs)

-- TODO discuss order of cases in:
-- foo :: Sing f -> Sing ls -> (Map f ls :~: '[]) -> ls :~: '[]
-- foo f SNil Refl = Refl
-- foo f (SCons a b) _ = undefined
--foo f ls Refl = case ls of SNil -> Refl

foo :: Sing f -> Sing ls -> (Map f ls :~: '[]) -> ls :~: '[]
foo f ls Refl = case ls of SNil -> Refl


-- skewCons :: forall l v fs. Field l v -> SkewRecord fs -> SkewRecord ('(l, v) : fs)
-- skewCons (Field v) r = case r of
   -- (SkewRecord HNil) -> SkewRecord $ HNode v HEmpty HEmpty `HCons` HNil
    --(SkewRecord vs@(HCons _ HNil)) -> SkewRecord (HCons (HNode v HEmpty HEmpty) vs)
    -- (SCons ta (SCons tb ts)) -> \(SkewRecord (va `HCons` vb `HCons` vs)) -> case sHeight ta %:== sHeight tb of
        -- STrue -> SkewRecord (HNode v va vb `HCons` vs)
        -- SFalse -> SkewRecord ((HNode v HEmpty HEmpty) `HCons` va `HCons` vb `HCons` vs)

        
l1 = sing :: Sing "L1"
l2 = sing :: Sing "L2"
l3 = sing :: Sing "L3"


-- cons = LCons
-- infixr 2 `cons`
-- nil = LNil
-- type Record = ListRecord

type Record = SkewRecord
cons :: _ => Field l v -> Record fs -> Record ('(l, v) : fs)
cons = skewCons
infixr 2 `cons`
nil = skewNil

r :: Record '[ '("L1", Int), '("L2", String)]
r = (Field 42) `cons` (Field "hi") `cons` nil

-- r :: Record '[ '("L2", String)]
-- r = (Field "hi") `cons` nil


-- class HSkewExtend f r r' | f r -> r'
    -- where hSkewExtend :: f -> r -> r'
-- infixr 2 `hSkewExtend`
-- instance
    -- (  HSkewExtend' (HSkewCarry r)  f r r',
       -- SingI (HSkewCarry r)) =>
       -- HSkewExtend                  f r r' where
    -- hSkewExtend f r =
        -- hSkewExtend' (hSkewCarry r) f r

-- class HSkewExtend' (b::Bool) f r r' | b f r -> r' where
    -- hSkewExtend' :: Sing b -> f -> r -> r'
-- instance
    -- HSkewExtend'
        -- 'False
        -- f
        -- r
        -- (HCons (HLeaf f) r) where
    -- hSkewExtend' _ f r = HCons (hLeaf f) r
-- instance
    -- HSkewExtend'
        -- 'True
        -- f
        -- (HCons t (HCons t' r))
        -- (HCons (HNode f t t') r) where
    -- hSkewExtend' _ f (HCons t (HCons t' r)) =
        -- (HCons (HNode f t t') r)

-- class HSkewGet r l v | r l -> v where
    -- hSkewGet :: r -> Sing l -> v
-- instance HSkewGet HNil l HNothing where
    -- hSkewGet _ _ = HNothing
-- instance HSkewGet HEmpty l HNothing where
    -- hSkewGet _ _ = HNothing
-- instance
    -- (  HSkewGet r   l vr
    -- ,  HSkewGet r'  l vr'
    -- ,  HPlus vr vr' v) =>
       -- HSkewGet (HCons r r') l v where
    -- hSkewGet (HCons r r') l =
        -- hSkewGet r l `hPlus` hSkewGet r' l
-- instance
    -- (  HSkewGet f   l vf
    -- ,  HSkewGet r   l vr
    -- ,  HSkewGet r'  l vr'
    -- ,  HPlus vf   vr     vfr
    -- ,  HPlus vfr  vr'  v) =>
       -- HSkewGet (HNode f r r') l v where
    -- hSkewGet (HNode f r r') l =
        -- hSkewGet f l
            -- `hPlus` hSkewGet r l
               -- `hPlus` hSkewGet r' l
-- instance
    -- (  HMakeMaybe (HEq l l') v m
    -- ,  SingI (HEq l l')
    -- ,  SingI l') =>
       -- HSkewGet (Field l' v) l m where
    -- hSkewGet f l =
        -- hMakeMaybe
            -- (hEq l (label f))
            -- (value f)

-- rSkew =
  -- ((sing :: Sing "L1")  .=.  True     )  `hSkewExtend`
  -- ((sing :: Sing "L2")  .=.  9        )  `hSkewExtend`
  -- ((sing :: Sing "L3")  .=.  "bla"    )  `hSkewExtend`
  -- ((sing :: Sing "L4")  .=.  'c'      )  `hSkewExtend`
  -- ((sing :: Sing "L5")  .=.  Nothing  )  `hSkewExtend`
  -- ((sing :: Sing "L6")  .=.  [4,5]    )  `hSkewExtend`
  -- ((sing :: Sing "L7")  .=.  "last"   )  `hSkewExtend`
  -- emptySkewRecord

-- class HPlus a b c | a b -> c where
    -- hPlus :: a -> b -> c
-- instance HPlus (HJust a) b (HJust a) where
    -- hPlus a  _ = a
-- instance HPlus HNothing b b where
    -- hPlus _  b = b

-- type family HSkewCarry (l :: HList fs) :: Bool where
  -- HSkewCarry HNil = 'False
  -- HSkewCarry (HCons t HNil) = 'False
  -- HSkewCarry (HCons t (HCons t' ts)) = HEq (HHeight t) (HHeight t')
-- hSkewCarry :: SingI (HSkewCarry l) => l -> Sing (HSkewCarry l)
-- hSkewCarry l = sing


-- type family HEq x y :: Bool where
  -- HEq x x = 'True
  -- HEq x y = 'False

-- hEq :: SingI (HEq x y) => Sing x -> Sing y -> Sing (HEq x y)
-- hEq x y = sing

-- newtype Field l v   =   Field { value :: v }
-- (.=.)               ::  Sing l -> v -> Field l v
-- _  .=.  v           =   Field v
-- label  :: SingI l => Field l v -> Sing l
-- label f =   sing

-- data HNothing  = HNothing
-- data HJust e   = HJust e deriving Show

-- class HMakeMaybe (b::Bool) v m | b v -> m where
    -- hMakeMaybe :: Sing b -> v -> m
-- instance HMakeMaybe 'False v HNothing where
    -- hMakeMaybe b v = HNothing
-- instance HMakeMaybe 'True v (HJust v) where
    -- hMakeMaybe b v = HJust v

-- type family HHeight t :: Nat where
  -- HHeight HEmpty = 0
  -- HHeight (HNode e t t') = 1 + HHeight t
