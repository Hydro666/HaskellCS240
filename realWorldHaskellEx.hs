lastButOne :: [a] -> a
lastButOne lst = if (length lst) > 1
                 then head (drop ((length lst) - 2) lst)
                 else error "The list is too short!"

data List a = Cons a (List a)
            | Nil deriving Show

data Tree a = Node a (Maybe (Tree a)) (Maybe (Tree a)) deriving Show

fromCons :: List a -> [a]
fromCons Nil = []
fromCons (Cons a as) = a : fromCons as
