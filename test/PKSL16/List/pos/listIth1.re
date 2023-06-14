/*@ measure elts : list('a) => Set_Set('a) */

/*@ measure len : list('a) => int */


type list('a) [v| len(v) >= 0] =
  | Nil                      => [v| v = Nil && elts(v) = Set_empty(0) && len(v) = 0]
  | Cons (x:'a, xs:list('a)) => [v| v = Cons(x,xs) && elts(v) = Set_add(elts(xs), x) && len(v) = 1 + len(xs)]
  ;


/*@ val at : xs:list('a) => i:int[v| 0<i && i<=len(xs)] => 'a[v| Set_mem(v, elts(xs))] / len(xs) */
let rec at = (xs, i) => {
  switch (xs) {
    | Nil        => impossible(0)
    | Cons(h, t) => let done = i==1;
                    if (done) {
                      h
                    } else {
                      let nexti = i - 1;
                      at(t, nexti)
                    }
  }
};
