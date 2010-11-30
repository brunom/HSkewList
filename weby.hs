import Prelude hiding (drop, head)

data List a
    = Nil
    | Cons a (Tree a) (List a) (List a)
data Tree a
    = Leaf
    | Node (List a) (List a)

height Leaf = 0
height (Node (Cons _ ta _ _) (Cons _ tb _ _))
    | ha == hb =  1 + ha
    | otherwise = error "unbalanced tree"
    where
      ha = height ta
      hb = height tb

size Leaf = 1
size (Node (Cons _ ta _ _) (Cons _ tb _ _))
    | sa == sb =  1 + sa + sb
    | otherwise = error "unbalanced tree"
    where
      sa = size ta
      sb = size tb

cons a Nil = Cons a Leaf Nil Nil
cons a bs@(Cons b _ Nil _) = Cons a Leaf bs bs
cons a bs@(Cons b tb cs@(Cons c tc ds _) _)
    | height tb == height tc = Cons a (Node bs cs) ds bs
    | otherwise = Cons a Leaf bs bs

head (Cons a _ _ _) = a

drop 0 as = as
drop n (Cons a t cs bs)
    | size t  <= n = drop (n-size t) cs
    | otherwise = drop (n-1) bs

pp n = map (\i -> head . drop i $ list) [0..n]
    where
      list = foldr cons Nil [0..n]
