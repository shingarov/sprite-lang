
/*@ measure len : list('a) => int */

type list('a) [v| len(v) >= 0] =
  | Nil                      => [v| v = Nil && len(v) = 0]
  | Cons (x:'a, xs:list('a)) => [v| v = Cons(x,xs) && len(v) = 1 + len(xs)]
  ;


/*@ val replicate : i:int[v|0<=v] => x:'a => list('a[v|v=x])[v| i=len(v)] / i */
let rec replicate = (i,x) => {
  let done = i==0;
  if (done) {
    Nil
  } else {
    let j = i - 1;
    let tail = replicate(j,x);
    Cons(x,tail)
  }
};
