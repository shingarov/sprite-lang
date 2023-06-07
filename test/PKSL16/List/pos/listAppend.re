
/*@ measure len : list('a) => int */


type list('a) [v| len(v) >= 0] =
  | Nil                      => [v| v = Nil && len(v) = 0]
  | Cons (x:'a, xs:list('a)) => [v| v = Cons(x,xs) && len(v) = 1 + len(xs)]
  ;

/*@ val append : xs:list('a) => ys:list('a) => list('a)[v| len(v) = len(xs) + len(ys) ] / len(xs) */
let rec append = (xs, ys) => {
  switch (xs) {
  | Nil        => ys 
  | Cons(h, t) => let rest = append(t, ys);
                  Cons(h, rest) 
  }
};
