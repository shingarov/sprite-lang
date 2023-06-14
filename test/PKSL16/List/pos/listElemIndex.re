
/*@ measure len : list('a) => int */
/*@ measure elems : list('a) => int */

type list('a)(p : int => 'a => bool) [v| len(v) >= 0] =
  | Nil                      => [v| v = Nil && len(v) = 0]
  | Cons (x:'a[v|p 0 v], xs:list('a)((j:int, b:'a) => p (j + 1) b)) => [v| v = Cons(x,xs) && len(v) = 1 + len(xs)]
  ;


/*@ val elemIndex : int => int */
let elemIndex = (x) => {
  42
};
