rev (xs) = rev' (xs, Nil())
rev' (xs,ys) =
  if xs : Nil
     then ys
     else rev' ( #Cons.2(xs)
               , Cons(#Cons.1(xs),ys))