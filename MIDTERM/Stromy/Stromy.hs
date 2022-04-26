import Data.Maybe


data BTree t = BNode (BTree t) t (BTree t) | Nil deriving (Eq, Show)
data Tree t = Node t [Tree t] deriving (Eq, Show)



bserialize::BTree t -> [Maybe t]
bserialize Nil = [Nothing]
bserialize (BNode l t p) = Just t : (bserialize l ++ bserialize p)


bdeserializeHelp::[Maybe t]-> (BTree t, [Maybe t])
bdeserializeHelp (s:ss) | isNothing s = (Nil, ss)
                        | otherwise = let Just x = s; (l, zv) = bdeserializeHelp ss; (r, zv2) = bdeserializeHelp zv in (BNode l x r, zv2)

bdeserialize::[Maybe t]-> BTree t
bdeserialize = fst.bdeserializeHelp


serialize::Tree t -> [Maybe t]
serialize (Node x z) = Just x : (concat [serialize c| c<-z]) ++ [Nothing]

deserializeHelp::[Maybe t]-> (Tree t, [Maybe t])
deserializeHelp (s:ss) = let Just x = s; (cdn, zv) = findChildren ss in (Node x cdn, zv)

findChildren::[Maybe t] -> ([Tree t], [Maybe t])
findChildren ss@(z:zv) = if isNothing z then ([], zv) else let (c, zv2) = deserializeHelp ss; (cdn, zv3) = findChildren zv2 in (c:cdn, zv3)

deserialize::[Maybe t]-> Tree t
deserialize = fst.deserializeHelp
