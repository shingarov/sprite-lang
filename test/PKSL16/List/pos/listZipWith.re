/*@ measure len : list('a) => int */

type list('a) [v| len(v) >= 0] =
  | Nil                      => [v| v = Nil && len(v) = 0]
  | Cons (x:'a, xs:list('a)) => [v| v = Cons(x,xs) && len(v) = 1 + len(xs)]
  ;


/*@ val zipWith : f:('a => 'b => 'c) => xs:list('a) => ys:list('b)[v|len(v) == len(xs)]  =>  list('c)[v|len(v)==len(xs)] / len(xs) */
let rec zipWith = (f,xs, ys) => {
  switch (xs) {
    | Nil        => Nil
    | Cons(h, t) => switch (ys) {
                      | Nil => impossible(0)
                      | Cons(hh, tt) => let newHead = f(h, hh);
                                        let newTail = zipWith(f, t, tt);
                                        Cons(newHead, newTail)
                    }
  }
};
