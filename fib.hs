{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

data HFalse
data HTrue

class Xor a b xab | a b -> xab
instance Xor HFalse HFalse HFalse
instance Xor HTrue HFalse HTrue
instance Xor HFalse HTrue HTrue
instance Xor HTrue HTrue HFalse

data HZero = HZero
data HSucc n = HSucc n

class Fib n b | n -> b
instance Fib HZero HTrue
instance Fib (HSucc HZero) HTrue
instance
    (Fib n bn
    ,Fib (HSucc n) bnn
    ,Xor bn bnn b)
    => Fib (HSucc (HSucc n)) b

class Bar b where bool :: b -> Bool
instance Bar HFalse where bool = const False
instance Bar HTrue where bool = const True

derp :: Fib n b => n -> b
derp n = (undefined::b)

main = print $ bool $ derp $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HSucc $
       HZero