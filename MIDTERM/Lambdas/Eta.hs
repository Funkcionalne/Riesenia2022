type Var = String

data LExp = LAMBDA Var LExp | ID Var | APP LExp LExp  deriving(Eq)

free :: LExp -> [Var]
free (ID x) = [x]
free (LAMBDA x exp) = filter (/=x) (free exp)
free (APP exp1 exp2) = free exp1 ++ free exp2

eta::LExp->LExp->Bool
eta (LAMBDA x1 (APP b1 (ID x2))) b2 | x1 == x2 && b1 == b2 && not (elem x2 (free b1)) = True 
                                    | x1 == x2 && eta b1 b2 = True
eta b2 (LAMBDA x1 (APP b1 (ID x2))) | x1 == x2 && b1 == b2 && not (elem x2 (free b1)) = True
                                    | x1 == x2 && eta b1 b2 = True
eta (APP e1 e2) (APP e3 e4) = eta e1 e3 && eta e2 e4 
eta (ID x) (ID y) = x == y 
eta (LAMBDA x exp) (LAMBDA x2 exp2) = x == x2 && eta exp exp2
eta _ _ = False  


-- eta (LAMBDA "f" (APP (ID "g") (LAMBDA "x" (APP (ID "f") (ID "x")))))  (ID "g")