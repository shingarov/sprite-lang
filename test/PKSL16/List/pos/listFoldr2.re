
/*@ measure len : list('a) => int */

type list('a) [v| len(v) >= 0] =
  | Nil                      => [v| v = Nil         && len(v) = 0]
  | Cons (x:'a, xs:list('a)) => [v| v = Cons(x,xs)  && len(v) = 1 + len(xs)]
  ;


/*@ val foldr : forall (p: list('a) => 'b => bool).
                   f:(xs:list('a) => x:'a => acc:'b[v|p xs v] => 'b[v|p (Cons x xs) v])  =>
                seed:'b[v|p (Nil) v]                                                       =>
                  ys:list('a)                                                            =>
                     'b[v|p ys v]                 / len(ys)    */
let rec foldr = (f, seed, ys) => {
  switch (ys) {
  | Nil        => seed
  | Cons(h, t) => seed
  }
};
