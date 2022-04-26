-- identifikator premennej je String 
type Var = String

-- lambda termy
data LExp = LAMBDA Var LExp | ID Var | APP LExp LExp  deriving(Eq)

-- 1 bod
-- premenna je volna v lambda terme

freeInTerm  :: Var -> LExp -> Bool
freeInTerm var (ID x) = var == x
freeInTerm var (APP m n) = freeInTerm var m || freeInTerm var n
freeInTerm var (LAMBDA x m) = var /= x && freeInTerm var m

eta :: LExp -> LExp -> Bool
eta e1 e2                       | e1 == e2  = True
eta (ID x) (ID y)                           = x == y
eta (APP e1 e2) (APP f1 f2)                 = eta e1 f1 && eta e2 f2
eta (LAMBDA x e1) (LAMBDA y e2) | x == y    = eta e1 e2
eta (LAMBDA x (APP e1 (ID y))) e2  | x == y = e1 == e2 && not (freeInTerm x e1)
eta e1 (LAMBDA x (APP e2 (ID y)))  | x == y = e1 == e2 && not (freeInTerm x e2)
eta _ _ = False

e = [ 
      eta (LAMBDA "x" (APP (ID "f") (ID "x"))) (ID "f"),
      eta (LAMBDA "x" (APP (ID "f") (ID "y"))) (ID "f"),
      eta (LAMBDA "x" (APP (ID "f") (ID "y"))) (LAMBDA "x" (APP (ID "f") (ID "y"))),
      eta (APP (LAMBDA "x" (APP (ID "f") (ID "y"))) (ID "z")) (APP (ID "f") (ID "z")),
      eta (APP (LAMBDA "x" (APP (ID "f") (ID "x"))) (ID "z")) (APP (ID "f") (ID "z"))
    ]

