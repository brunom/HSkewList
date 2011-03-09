{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Data.HList hiding ((.*.))
import Skew
myNil = hSkewNil

--import Data.HList
--import Skew hiding ((.*.))
--myNil = hNil

data L1; l1 = undefined :: Proxy L1
data L2; l2 = undefined :: Proxy L2

main = go (99999999::Int) where
    go i = if i == 0 then return() else go (i - make i # l2)

{-# NOINLINE make #-}
make i = list

list =
    l1 .=. (0::Int) .*.
    l1 .=. (0::Int) .*.
    l1 .=. (0::Int) .*.
    l1 .=. (0::Int) .*.
    l1 .=. (0::Int) .*.
    l1 .=. (0::Int) .*.
    l1 .=. (0::Int) .*.
    l1 .=. (0::Int) .*.
    l1 .=. (0::Int) .*.
    l1 .=. (0::Int) .*.
    l1 .=. (0::Int) .*.
    l1 .=. (0::Int) .*.
    l1 .=. (0::Int) .*.
    l1 .=. (0::Int) .*.
    l1 .=. (0::Int) .*.
    l1 .=. (0::Int) .*.
    l1 .=. (0::Int) .*.
    l1 .=. (0::Int) .*.
    l1 .=. (0::Int) .*.
    l1 .=. (0::Int) .*.
    l1 .=. (0::Int) .*.
    l1 .=. (0::Int) .*.
    l1 .=. (0::Int) .*.
    l1 .=. (0::Int) .*.
    l1 .=. (0::Int) .*.
    l1 .=. (0::Int) .*.
    l1 .=. (0::Int) .*.
    l1 .=. (0::Int) .*.
    l1 .=. (0::Int) .*.
    l1 .=. (0::Int) .*.
    l1 .=. (0::Int) .*.
    l1 .=. (0::Int) .*.
    l1 .=. (0::Int) .*.
    l1 .=. (0::Int) .*.
    l1 .=. (0::Int) .*.
    l1 .=. (0::Int) .*.
    l1 .=. (0::Int) .*.
    l1 .=. (0::Int) .*.
    l1 .=. (0::Int) .*.
    l1 .=. (0::Int) .*.
    l1 .=. (0::Int) .*.
    l1 .=. (0::Int) .*.
    l1 .=. (0::Int) .*.
    l1 .=. (0::Int) .*.
    l1 .=. (0::Int) .*.
    l1 .=. (0::Int) .*.
    l1 .=. (0::Int) .*.
    l1 .=. (0::Int) .*.
    l1 .=. (0::Int) .*.
    l1 .=. (0::Int) .*.
    l2 .=. (1::Int) .*.
    myNil
