{-#  LANGUAGE EmptyDataDecls  #-}
{-#  LANGUAGE FlexibleContexts  #-}
{-#  LANGUAGE TypeOperators  #-}
{-#  LANGUAGE MultiParamTypeClasses  #-}
{-#  LANGUAGE FunctionalDependencies  #-}
{-#  LANGUAGE FlexibleInstances  #-}
{-#  LANGUAGE UndecidableInstances  #-}
{-#  LANGUAGE OverlappingInstances  #-}

main = main

data HNil       = HNil
data HCons e l  = HCons e l

data  HEmpty           =  HEmpty
data  HNode  e  t  t'  =  HNode  e  t  t'
type  HLeaf  e         =  HNode e HEmpty HEmpty

hLeaf        e         =  HNode e HEmpty HEmpty

newtype SkewRecord r = SkewRecord r
emptySkewRecord :: SkewRecord HNil
emptySkewRecord = SkewRecord HNil

class HUpdate l e r r' | l e r -> r' where
    hUpdate :: l -> e -> r -> r'

class HSkewUpdate l e r r' | l e r -> r' where
    hSkewUpdate :: l -> e -> r -> r'

instance HSkewUpdate l e HNil HNil where
    hSkewUpdate _ _ = id
instance HSkewUpdate l e HEmpty HEmpty where
    hSkewUpdate _ _ = id

instance
    (  HSkewUpdate l e t t'
    ,  HSkewUpdate l e ts ts') =>
       HSkewUpdate l e (HCons t ts) (HCons t' ts') where
    hSkewUpdate l e (HCons t ts) =
        HCons
            (hSkewUpdate l e t)
            (hSkewUpdate l e ts)

instance
    (  HSkewUpdate l e e' e''
    ,  HSkewUpdate l e tl tl'
    ,  HSkewUpdate l e tr tr') =>
       HSkewUpdate l e (HNode e' tl tr) (HNode e'' tl' tr')
       where
    hSkewUpdate l e (HNode e' tl tr) =
        HNode
            (hSkewUpdate l e e')
            (hSkewUpdate l e tl)
            (hSkewUpdate l e tr)

class HSkewTail ts ts' | ts -> ts' where
    hSkewTail :: ts -> ts'


class HRemove l r r' | l r -> r' where
    hRemove :: l -> r -> r'
instance
    (  HSkewUpdate l e (HCons (HNode e t t') ts) r'
    ,  HSkewTail r' r'') =>
       HRemove
        l
        (SkewRecord (HCons (HNode e t t') ts))
        (SkewRecord r'') where
    hRemove l (SkewRecord (HCons (HNode e t t') ts)) =
        SkewRecord $
        hSkewTail $
        hSkewUpdate l e (HCons (HNode e t t') ts)
