/*@ measure len : list('a) => int */

type list('a) [v| len(v) >= 0] =
  | Nil                      => [v| v = Nil && len(v) = 0]
  | Cons (x:'a, xs:list('a)) => [v| v = Cons(x,xs) && len(v) = 1 + len(xs)]
  ;

/*@ val null : xs:list('a) => bool[b|b <=> (0==len(xs))] */
let null = (xs) => {
  switch (xs) {
    | Nil        => true
    | Cons(h, t) => false
  }
};
