import Challenges

-- >>> prettyPrint(Var 1)

-- >>> prettyPrint(App (Var 1) (Var 1))

-- >>> prettyPrint(App (App (Var 1) (Var 1))(Var 1)) 

-- >>> prettyPrint(App  (Let (V 1)  (Var 1) (Var 1)) (Var 1) )

-- >>> prettyPrint(App (Pair (Var 1) (Var 1)) (Var 1) )

-- >>> prettyPrint(App (Fst (Var 1)) (Var 1) )

-- >>> prettyPrint(App (Snd (Var 1)) (Var 1) )

-- >>> prettyPrint(App (Abs (V 1) (Var 1)) (Var 1) )

-- >>> prettyPrint(App (Var 1) (App (Var 1) (Var 1)))

-- >>> prettyPrint(App  (Var 1)(Let (V 1)  (Var 1) (Var 1)) )

-- >>> prettyPrint(App  (Var 1) (Pair (Var 1) (Var 1)))

-- >>> prettyPrint(App  (Var 1) (Fst (Var 1)))

-- >>> prettyPrint(App  (Var 1) (Snd (Var 1)))

-- >>> prettyPrint(App  (Var 1) (Abs (V 1) (Var 1)))

-- >>> prettyPrint(Let (V 1)  (Var 1) (Var 1))

-- >>> prettyPrint(Let Discard  (Var 1) (Var 1))

-- >>> prettyPrint(Let (V 1)  (App (Var 1) (Var 1)) (Var 1))

-- >>> prettyPrint(Let (V 1)  (Let (V 1)  (Var 1) (Var 1)) (Var 1))

-- >>> prettyPrint(Let (V 1)  (Pair (Var 1) (Var 1)) (Var 1))

-- >>> prettyPrint(Let (V 1)  (Fst (Var 1)) (Var 1))

-- >>> prettyPrint(Let (V 1)  (Snd (Var 1)) (Var 1))

-- >>> prettyPrint(Let (V 1)  (Abs (V 1) (Var 1)) (Var 1))

-- >>> prettyPrint(Let (V 1)   (Var 1)(App (Var 1) (Var 1)))

-- >>> prettyPrint(Let (V 1)   (Var 1)(Let (V 1)  (Var 1) (Var 1)))

-- >>> prettyPrint(Let (V 1)   (Var 1)(Pair (Var 1) (Var 1)))

-- >>> prettyPrint(Let (V 1)   (Var 1)(Fst (Var 1)))

-- >>> prettyPrint(Let (V 1)   (Var 1)(Snd (Var 1)))

-- >>> prettyPrint(Let (V 1)   (Var 1)(Abs (V 1) (Var 1)))

-- >>> prettyPrint(Pair (Var 1) (Var 1))

-- >>> prettyPrint(Pair (App (Var 1) (Var 1))(Var 1) )

-- >>> prettyPrint(Pair (Let (V 1)  (Var 1) (Var 1)) (Var 1) )

-- >>> prettyPrint(Pair (Pair (Var 1) (Var 1)) (Var 1) )

-- >>> prettyPrint(Pair (Fst (Var 1)) (Var 1) )

-- >>> prettyPrint(Pair (Snd (Var 1)) (Var 1) )

-- >>> prettyPrint(Pair (Abs (V 1) (Var 1)) (Var 1) )

-- >>> prettyPrint(Pair (Var 1) (App (Var 1) (Var 1)))

-- >>> prettyPrint(Pair  (Var 1)(Let (V 1)  (Var 1) (Var 1)) )

-- >>> prettyPrint(Pair  (Var 1) (Pair (Var 1) (Var 1)))

-- >>> prettyPrint(Pair  (Var 1) (Fst (Var 1)))

-- >>> prettyPrint(Pair  (Var 1) (Snd (Var 1)))

-- >>> prettyPrint(Pair  (Var 1) (Abs (V 1) (Var 1)))

-- >>> prettyPrint(Fst (Var 1))

-- >>> prettyPrint(Fst (App (Var 1) (Var 1)))

-- >>> prettyPrint(Fst (Pair (Let (V 1)  (Var 1) (Var 1)) (Var 1)) )

-- >>> prettyPrint(Fst (Pair (Var 1) (Var 1)))

-- >>> prettyPrint(Fst (Fst (Var 1)))

-- >>> prettyPrint(Fst (Snd (Var 1)))

-- >>> prettyPrint(Fst (Abs (V 1) (Var 1)))

-- >>> prettyPrint(Snd (Var 1))

-- >>> prettyPrint(Snd (App (Var 1) (Var 1)))

-- >>> prettyPrint(Snd (Pair (Let (V 1)  (Var 1) (Var 1)) (Var 1) ))

-- >>> prettyPrint(Snd (Pair (Var 1) (Var 1)))

-- >>> prettyPrint(Snd (Fst (Var 1)))

-- >>> prettyPrint(Snd (Snd (Var 1)))

-- >>> prettyPrint(Snd (Abs (V 1) (Var 1)))

-- >>> prettyPrint(Abs (V 1) (Var 1))

-- >>> prettyPrint(Abs Discard (Var 1))

-- >>> prettyPrint(Abs (V 1) (App (Var 1) (Var 1)))

-- >>> prettyPrint(Abs (V 1) (Let (V 1)  (Var 1) (Var 1)))

-- >>> prettyPrint(Abs (V 1) (Pair (Var 1) (Var 1)))

-- >>> prettyPrint(Abs (V 1) (Fst (Var 1)))

-- >>> prettyPrint(Abs (V 1) (Snd (Var 1)))

-- >>> prettyPrint(Abs (V 1) (Abs (V 1) (Var 1)))