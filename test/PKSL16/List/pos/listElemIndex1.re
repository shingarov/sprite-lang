
/*@ measure len : list('a) => int */
/*@ measure elems : list('a) => int */

type list('a)(p : int => 'a => bool) [v| len(v) >= 0] =
  | Nil                      => [v| v = Nil && len(v) = 0]
  | Cons (x:'a[v|p 0 v], xs:list('a)((j:int, b:'a) => p (j + 1) b)) => [v| v = Cons(x,xs) && len(v) = 1 + len(xs)]
  ;


/*@ val elemIndex : forall (p: int => 'a => bool).  x:'a => xs:list('a)(p) => int / len(xs) */
let rec elemIndex = (x,xs) => {
  switch (xs) {
    | Nil => impossible(0)
    | Cons(h,t) => let j = i - 1;
                   let newTail = take(j,t);
                   Cons(h,newTail)
  }
};
