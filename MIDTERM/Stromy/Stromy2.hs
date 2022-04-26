data BTree t = BNode (BTree t) t (BTree t) | Nil deriving (Show)

bserialize :: BTree t -> [Maybe t]
bserialize Nil = [Nothing]
bserialize (BNode l v r) = (Just v:bserialize l) ++ bserialize r

bdeserialize xs = t
                 where  (t, _) = bdeserialize' xs
                        bdeserialize' (Nothing:xs) = (Nil,xs)
                        bdeserialize' (Just v:xs) = let (left, xs') = bdeserialize' xs
                                                        (right, xs'') = bdeserialize' xs'
                                                     in ((BNode left v right), xs'')
                                                   
t = BNode  (BNode Nil 3 Nil) 5 (BNode Nil 6 Nil) 

data Tree t = Node t [Tree t] deriving (Show)
serialize (Node root sons) = Just root : serialize' sons
serialize' [] = [Nothing]
serialize' (x:xs) = (serialize x) ++ serialize' xs

tt = Node 3 [Node 2 [], Node 5 [], Node 7 []]
                                                   