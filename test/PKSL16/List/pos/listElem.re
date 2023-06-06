/*@ measure elts : list('a) => Set_Set('a) */

/*@ measure len : list('a) => int */


type list('a) [v| len(v) >= 0] =
  | Nil                      => [v| v = Nil && elts(v) = Set_empty(0) && len(v) = 0]
  | Cons (x:'a, xs:list('a)) => [v| v = Cons(x,xs) && elts(v) = Set_add(elts(xs), x) && len(v) = 1 + len(xs)]
  ;

/*@ val elemOf : x:'a => xs:list('a) => bool[v|v <=> Set_mem(x, elts(xs))] / len(xs) */
let rec elemOf = (x, xs) => {
  switch (xs) {
  | Nil        => false 
  | Cons(h, t) => let found = x==h;
                  if (found) {
                    true
                  } else {
                    elemOf(x, t)
                  }
  }
};
