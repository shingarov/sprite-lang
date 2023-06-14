type list('a) =
  | Nil
  | Cons (x:'a, xs:list('a))
  ;

/*@ val foldr : forall (p: list('a) => 'b => bool).
                (xs:list('a) => x:'a => acc:'b[v|p xs v] => 'b[v|p (Cons x xs) v])  =>
                seed:'b[v|p (Nil) v]                                                =>
                ys:list('a)                                                         =>
                'b[v|p ys v]                  */
let rec foldr = (op, seed, ys) => {
  switch (ys) {
  | Nil        => seed
  | Cons(h, t) => let nextb = foldr(op, seed, t);
                  op(t, h, nextb)
  }
};
