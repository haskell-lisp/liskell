module Class where

class Bar a where
    kuh :: a -> Int

data Tree a =  Leaf a
            |  Branch (Tree a) (Tree a)

type Lala = Char

instance Bar Lala where
    kuh a = 1

main = print (kuh "a")