{-# LINE 25 "paper.lhs" #-}
{-#  LANGUAGE EmptyDataDecls  #-}
{-#  LANGUAGE FlexibleContexts  #-}
{-#  LANGUAGE TypeFamilies  #-}
{-#  LANGUAGE TypeOperators  #-}
{-#  LANGUAGE MultiParamTypeClasses  #-}
{-#  LANGUAGE FunctionalDependencies  #-}
{-#  LANGUAGE FlexibleInstances  #-}
{-#  LANGUAGE UndecidableInstances  #-}
-- {-# LANGUAGE TypeInType #-}
{-#  LANGUAGE GADTs  #-}
{-#  OPTIONS_GHC -Wunticked-promoted-constructors  #-}

module Paper where

import Data.Array
import GHC.Exts
import Unsafe.Coerce
import Data.Kind

hListUpdate a = hSkewUpdate a
hListRemove = undefined
{-# LINE 167 "paper.lhs" #-}
data   HTrue   ; hTrue   = undefined :: HTrue
data   HFalse  ; hFalse  = undefined :: HFalse
{-# LINE 178 "paper.lhs" #-}
class HNot t t' | t -> t' where
  hNot :: t -> t'
{-# LINE 190 "paper.lhs" #-}
instance HNot  HFalse  HTrue   where hNot _ = hTrue
instance HNot  HTrue   HFalse  where hNot _ = hFalse
{-# LINE 202 "paper.lhs" #-}
data HNothing  = HNothing
data HJust e   = HJust e deriving Show
{-# LINE 211 "paper.lhs" #-}
class HMakeMaybe b v m | b v -> m where
    hMakeMaybe :: b -> v -> m
instance HMakeMaybe HFalse v HNothing where
    hMakeMaybe b v = HNothing
instance HMakeMaybe HTrue v (HJust v) where
    hMakeMaybe b v = HJust v
{-# LINE 223 "paper.lhs" #-}
class HPlus a b c | a b -> c where
    hPlus :: a -> b -> c
instance HPlus (HJust a) b (HJust a) where
    hPlus a  _ = a
instance HPlus HNothing b b where
    hPlus _  b = b
{-# LINE 237 "paper.lhs" #-}
-- data HList (l::[*]) where
--     HNil  :: HList '[]
--     HCons :: e -> HList l -> HList (e ': l)
data HNil       = HNil
data HCons e l  = HCons e l
infixr 2 `HCons`
{-# LINE 255 "paper.lhs" #-}
newtype Field l v   =   Field { value :: v }
(.=.)               ::  l -> v -> Field l v
_  .=.  v           =   Field v
{-# LINE 266 "paper.lhs" #-}
label  ::  Field l v -> l
label  =   undefined
{-# LINE 273 "paper.lhs" #-}
data L1 = L1
data L2 = L2
data L3 = L3
data L4 = L4
data L5 = L5
data L6 = L6
data L7 = L7
{-# LINE 285 "paper.lhs" #-}
rList =
  (L1  .=.  True     )  `HCons`
  (L2  .=.  9        )  `HCons`
  (L3  .=.  "bla"    )  `HCons`
  (L4  .=.  'c'      )  `HCons`
  (L5  .=.  Nothing  )  `HCons`
  (L6  .=.  [4,5]    )  `HCons`
  (L7  .=.  "last"   )  `HCons`
  HNil
{-# LINE 300 "paper.lhs" #-}
class HListGet r l v | r l -> v where
    hListGet :: r -> l -> v
{-# LINE 311 "paper.lhs" #-}
lastList = hListGet rList L7
{-# LINE 321 "paper.lhs" #-}
class HEq x y b | x y -> b
hEq :: HEq x y b => x -> y -> b
hEq = undefined
{-# LINE 331 "paper.lhs" #-}
instance {-#  OVERLAPPING   #-}  HEq x x HTrue
instance b ~ HFalse =>         HEq x y b
{-# LINE 350 "paper.lhs" #-}
instance
    (  HEq l l' b
    ,  HListGet' b v' r' l v) =>
       HListGet (HCons (Field l' v') r') l v where
    hListGet (HCons f'@(Field v') r') l =
        hListGet' (hEq l (label f')) v' r' l
{-# LINE 361 "paper.lhs" #-}
class HListGet' b v' r' l v | b v' r' l -> v where
    hListGet':: b -> v' -> r' -> l -> v

instance
    HListGet' HTrue v r' l v
    where
    hListGet' _ v _ _ = v

instance
    HListGet r' l v =>
    HListGet' HFalse v' r' l v where
    hListGet' _ _ r' l = hListGet r' l
{-# LINE 391 "paper.lhs" #-}
lastListCore = case rList of
  HCons _ rs1 -> case rs1 of
    HCons _  rs2 -> case rs2 of
      HCons _  rs3 -> case rs3 of
        HCons _ rs4 -> case rs4 of
          HCons _ rs5 -> case rs5 of
            HCons _ rs6 -> case rs6 of
              HCons e _ -> e
{-# LINE 483 "paper.lhs" #-}
data ArrayRecord r =
  ArrayRecord r (Array Int Any)
{-# LINE 492 "paper.lhs" #-}
class ArrayFind r l v | r l -> v where
  arrayFind :: r -> l -> Int
{-# LINE 500 "paper.lhs" #-}
hArrayGet :: ArrayFind r l v => ArrayRecord r -> l -> v
hArrayGet (ArrayRecord r a) l =
  unsafeCoerce (a ! arrayFind r l)
{-# LINE 520 "paper.lhs" #-}
instance  (  HEq l l' b
          ,  ArrayFind' b v' r l v n
          ,  ToValue n) =>
   ArrayFind (HCons (Field l' v') r) l v where
     arrayFind (HCons f r) l =
       toValue (arrayFind' (hEq l (label f)) (value f) r l)
{-# LINE 535 "paper.lhs" #-}
arrayFind' ::  ArrayFind' b v' r l v n
               => b -> v' -> r -> l -> n
arrayFind' = undefined

data HZero
data HSucc n

class ArrayFind' b v' r l v n | b v' r l -> v n
instance ArrayFind' HTrue v r l v HZero
instance (HEq l l' b, ArrayFind' b v' r l v n)
         => ArrayFind'  HFalse v'' (HCons (Field l' v') r) l
                        v (HSucc n)
{-# LINE 557 "paper.lhs" #-}
class ToValue n where
  toValue :: n -> Int
{-# LINE 581 "paper.lhs" #-}
instance ToValue HZero where
  toValue _ = 0

hPrev :: HSucc n -> n
hPrev = undefined

instance ToValue n => ToValue (HSucc n) where
  toValue n = 1 + toValue (hPrev n)
{-# LINE 665 "paper.lhs" #-}
emptyArrayRecord =
  ArrayRecord HNil (array (0, -1) [])
{-# LINE 672 "paper.lhs" #-}
hArrayExtend f = hArrayModifyList (HCons f)

hArrayModifyList hc (ArrayRecord r _) =
  let  r'  = hc r
       fs  = hMapAny r'
  in   ArrayRecord r' (listArray (0, length fs - 1) fs)
{-# LINE 702 "paper.lhs" #-}
class HMapAny r where
  hMapAny :: r -> [Any]
instance HMapAny HNil where
  hMapAny _ = []
instance
  HMapAny r =>
  HMapAny (HCons (Field l v) r)
  where
  hMapAny (HCons (Field v) r) =
    unsafeCoerce v : hMapAny r
{-# LINE 724 "paper.lhs" #-}
hArrayUpdate l e
   = hArrayModifyList (hListUpdate l e)

hArrayRemove l
   = hArrayModifyList (hListRemove l)
{-# LINE 783 "paper.lhs" #-}
data  HEmpty           =  HEmpty
data  HNode  e  t  t'  =  HNode  e  t  t'
type  HLeaf  e         =  HNode e HEmpty HEmpty
{-# LINE 789 "paper.lhs" #-}
hLeaf        e         =  HNode e HEmpty HEmpty
{-# LINE 802 "paper.lhs" #-}
four =
    HCons  (hLeaf  (L4  .=.  'c')) $
    HCons  (HNode  (L5  .=.  Nothing)
                   (hLeaf (L6  .=.  [4,5]))
                   (hLeaf (L7  .=.  "last"))) $
    HNil
{-# LINE 817 "paper.lhs" #-}
emptySkewRecord = HNil
{-# LINE 825 "paper.lhs" #-}
class HHeight t h | t -> h
instance  HHeight HEmpty HZero
instance  HHeight t h =>
          HHeight (HNode e t t') (HSucc h)
{-# LINE 843 "paper.lhs" #-}
class HSkewCarry l b | l -> b

hSkewCarry :: HSkewCarry l b => l -> b
hSkewCarry = undefined
{-# LINE 851 "paper.lhs" #-}
instance HSkewCarry HNil HFalse
instance HSkewCarry (HCons t HNil) HFalse
{-# LINE 860 "paper.lhs" #-}
instance
    (  HHeight t h
    ,  HHeight t' h'
    ,  HEq h h' b) =>
       HSkewCarry (HCons t (HCons t' ts)) b
{-# LINE 870 "paper.lhs" #-}
class HSkewExtend f r r' | f r -> r'
    where hSkewExtend :: f -> r -> r'
infixr 2 `hSkewExtend`
{-# LINE 881 "paper.lhs" #-}
instance
    (  HSkewCarry r b
    ,  HSkewExtend' b  f r r') =>
       HSkewExtend     f r r' where
    hSkewExtend f r =
        hSkewExtend' (hSkewCarry r) f r

class HSkewExtend' b f r r' | b f r -> r' where
    hSkewExtend' :: b -> f -> r -> r'
{-# LINE 896 "paper.lhs" #-}
instance
    HSkewExtend'
        HFalse
        f
        r
        (HCons (HLeaf f) r) where
    hSkewExtend' _ f r = HCons (hLeaf f) r
{-# LINE 909 "paper.lhs" #-}
instance
    HSkewExtend'
        HTrue
        f
        (HCons t (HCons t' r))
        (HCons (HNode f t t') r) where
    hSkewExtend' _ f (HCons t (HCons t' r)) =
        (HCons (HNode f t t') r)
{-# LINE 927 "paper.lhs" #-}
class HSkewGet r l v | r l -> v where
    hSkewGet :: r -> l -> v
{-# LINE 946 "paper.lhs" #-}
instance HSkewGet HNil l HNothing where
    hSkewGet _ _ = HNothing
instance HSkewGet HEmpty l HNothing where
    hSkewGet _ _ = HNothing
{-# LINE 957 "paper.lhs" #-}
instance
    (  HSkewGet r   l vr
    ,  HSkewGet r'  l vr'
    ,  HPlus vr vr' v) =>
       HSkewGet (HCons r r') l v where
    hSkewGet (HCons r r') l =
        hSkewGet r l `hPlus` hSkewGet r' l
{-# LINE 977 "paper.lhs" #-}
instance
    (  HSkewGet f   l vf
    ,  HSkewGet r   l vr
    ,  HSkewGet r'  l vr'
    ,  HPlus vf   vr     vfr
    ,  HPlus vfr  vr'  v) =>
       HSkewGet (HNode f r r') l v where
    hSkewGet (HNode f r r') l =
        hSkewGet f l
            `hPlus` hSkewGet r l
               `hPlus` hSkewGet r' l
{-# LINE 998 "paper.lhs" #-}
instance
    (  HEq l l' b
    ,  HMakeMaybe b v m) =>
       HSkewGet (Field l' v) l m where
    hSkewGet f l =
        hMakeMaybe
            (hEq l (label f))
            (value f)
{-# LINE 1015 "paper.lhs" #-}
rSkew =
  (L1  .=.  True     )  `hSkewExtend`
  (L2  .=.  9        )  `hSkewExtend`
  (L3  .=.  "bla"    )  `hSkewExtend`
  (L4  .=.  'c'      )  `hSkewExtend`
  (L5  .=.  Nothing  )  `hSkewExtend`
  (L6  .=.  [4,5]    )  `hSkewExtend`
  (L7  .=.  "last"   )  `hSkewExtend`
  emptySkewRecord

lastSkew = hSkewGet rSkew L7
{-# LINE 1030 "paper.lhs" #-}
lastSkewCore = case rSkew of
  HCons t1 _ -> case t1 of
    HNode _ _ t12 -> case t12 of
      HNode _ _ t121 ->case t121 of
        HNode e _ _ -> e
{-# LINE 1050 "paper.lhs" #-}
class HSkewUpdate l e r r' | l e r -> r' where
    hSkewUpdate :: l -> e -> r -> r'
{-# LINE 1058 "paper.lhs" #-}
instance  (  HSkewGet r l m
          ,  HSkewUpdate' m l e r r') =>
          HSkewUpdate l e r r'  where
    hSkewUpdate l e r =
       hSkewUpdate' (hSkewGet r l) l e r

class HSkewUpdate' m l e r r' | m l e r -> r' where
    hSkewUpdate' :: m -> l -> e -> r -> r'
{-# LINE 1072 "paper.lhs" #-}
instance HSkewUpdate' HNothing l e r r  where
    hSkewUpdate' _ l e r = r
{-# LINE 1081 "paper.lhs" #-}
instance
    (  HSkewUpdate l e t t'
    ,  HSkewUpdate l e ts ts') =>
    HSkewUpdate' (HJust v) l e  (HCons t ts)
                                (HCons t' ts')
    where
    hSkewUpdate' _ l e (HCons t ts) =
        HCons  (hSkewUpdate l e t)
               (hSkewUpdate l e ts)
{-# LINE 1095 "paper.lhs" #-}
instance
    (  HSkewUpdate l e e' e''
    ,  HSkewUpdate l e tl tl'
    ,  HSkewUpdate l e tr tr') =>
    HSkewUpdate' (HJust v) l e  (HNode e' tl tr)
                                (HNode e'' tl' tr')
    where
    hSkewUpdate' _ l e (HNode e' tl tr) =
        HNode  (hSkewUpdate l e e')
               (hSkewUpdate l e tl)
               (hSkewUpdate l e tr)
{-# LINE 1112 "paper.lhs" #-}
instance
    HSkewUpdate' (HJust v) l e (Field l v) e
     where
       hSkewUpdate' _ l e e' = e
{-# LINE 1134 "paper.lhs" #-}
class HSkewTail ts ts' | ts -> ts' where
    hSkewTail :: ts -> ts'
{-# LINE 1152 "paper.lhs" #-}
instance HSkewTail (HCons (HLeaf e) ts) ts where
    hSkewTail (HCons _ ts) = ts
{-# LINE 1165 "paper.lhs" #-}
instance
    HSkewTail
        (HCons (HNode e t (HNode e' t' t'')) ts)
        (HCons t ((HCons (HNode e' t' t'')) ts))
    where
    hSkewTail (HCons (HNode _ t t') ts) =
        HCons t (HCons t' ts)
{-# LINE 1180 "paper.lhs" #-}
hSkewRemove l (HCons (HNode e t t') ts) =
        hSkewTail $
        hSkewUpdate l e (HCons (HNode e t t') ts)
