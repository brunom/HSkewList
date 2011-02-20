import Prelude hiding (drop, head, length)

data List a
    = Nil
    | Cons a (List a) (List a) (List a)

length Nil = 0
length (Cons _ xs _ _) = 1 + length xs

cons a Nil = Cons a Nil Nil Nil
cons a bs@(Cons b _ _ Nil) = Cons a bs bs bs
cons a bs@(Cons b _ _ es@(Cons e _ _ hs))
    | length bs - length es == length es - length hs = Cons a bs es hs
    | otherwise = Cons a bs bs bs

head (Cons a _ _ _) = a

drop :: Int -> List a -> List a
drop 0 as = as
drop n (Cons a bs cs ds)
    | n < las-lcs = drop (n-(las-lbs)) bs
    | n < las-lds = drop (n-(las-lcs)) cs
    | otherwise = drop (n-(las-lds)) ds
      where
        las = lbs + 1
        lbs = length bs
        lcs = length cs
        lds = length ds

pp n = map (\i -> head . drop i $ list) [0..n]
    where
      list = foldr cons Nil [0..n]
