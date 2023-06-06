/*@ measure len : list('a) => int */

type pair('p, 'q) =
  | MkPair(x:'p, y:'q)
  ;

type list('a) [v| len(v) >= 0] =
  | Nil                      => [v| v = Nil && len(v) = 0]
  | Cons (x:'a, xs:list('a)) => [v| v = Cons(x,xs) && len(v) = 1 + len(xs)]
  ;

/*@ val zip : xs:list('a) => ys:list('b)[v|len(v) == len(xs)]  =>  list(pair('a,'b))[v|len(v)==len(xs)] / len(xs) */
let rec zip = (xs, ys) => {
  switch (xs) {
    | Nil        => Nil
    | Cons(h, t) => switch (ys) {
                      | Nil => impossible(0)
                      | Cons(hh, tt) => let newHead = MkPair(h, hh);
                                        let newTail = zip(t, tt);
                                        Cons(newHead, newTail)
                    }
  }
};
