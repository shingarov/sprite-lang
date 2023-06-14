/*@ measure elts : list('a) => Set_Set('a) */

/*@ measure len : list('a) => int */


type list('a) [v| len(v) >= 0] =
  | Nil                      => [v| v = Nil && elts(v) = Set_empty(0) && len(v) = 0]
  | Cons (x:'a, xs:list('a)) => [v| v = Cons(x,xs) && elts(v) = Set_add(elts(xs), x) && len(v) = 1 + len(xs)]
  ;


/*@ val at : i:int[v| 0<i] => xs:list('a)[v| i<=len(xs)] => 'a / len(xs) */
let rec at = (i, xs) => {
  let done = i<2;
  if (done) {
    switch (xs) {
      | Nil        => impossible(0)
      | Cons(h, t) => h
    }
  } else {
    switch (xs) {
      | Nil        => impossible(0)
      | Cons(h, t) => switch (t) {
                        | Nil   => impossible(0)
                        | Cons (hh,tt) => let j = i - 1;
                                          at(j, tt)
                      }
    }
  }
};
