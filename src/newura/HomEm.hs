module HomEm where

import Language

class HomEm t where (<|) :: t -> t -> Bool

instance HomEm (Aexp d) where
  (AtomA s) <| (AtomA s') | s == s' = True
  (VarA _) <| (VarA _) = True
  _ <| _ = False

instance HomEm (Exp d) where
  (VarE _) <| (VarE _) = True
  (ConsE e1 e2) <| (ConsE e1' e2')
      | e1 <| e1' && e2 <| e2' = True
  e <| (ConsE e1' e2')
      | e <| e1' || e <| e2' = True
  (Aexp' a) <| (Aexp' a') | a <| a' = True
  _ <| _ = False

instance HomEm (Term d) where
  (FAppT f es) <| (FAppT f' es')
      | f == f' && and (zipWith (<|) es es') = True
  (GAppT g es) <| (GAppT g' es')
      | g == g' && and (zipWith (<|) es es') = True
  (IfT ea1 ea2 t1 t2) <| (IfT ea1' ea2' t1' t2')
      | ea1 <| ea1' && ea2 <| ea2' && t1 <| t1' && t2 <| t2' = True
  t <| (IfT _ _ t1' t2')
      | t <| t1' || t <| t2' = True
  (ExpT e) <| (IfT ea1' ea2' _ _)
      | e <| (Aexp' ea1') || e <| (Aexp' ea2') = True
  (ExpT e) <| (ExpT e')
      | e <| e' = True
  (ExpT e) <| (FAppT _ es')
      | any (e<|) es' = True
  (ExpT e) <| (GAppT _ es')
      | any (e<|) es' = True
  _ <| _ = False