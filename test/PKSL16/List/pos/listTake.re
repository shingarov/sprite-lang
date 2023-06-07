
/*@ measure len : list('a) => int */

type list('a) [v| len(v) >= 0] =
  | Nil                      => [v| v = Nil && len(v) = 0]
  | Cons (x:'a, xs:list('a)) => [v| v = Cons(x,xs) && len(v) = 1 + len(xs)]
  ;


/*@ val take : i:int[v|0<=v] => list('a)[v| i<=len(v)] => list('a)[v| i=len(v)] / i */
let rec take = (i,xs) => {
  let done = i==0;
  if (done) {
    Nil
  } else {
    switch (xs) {
      | Nil => impossible(0)
      | Cons(h,t) => let j = i - 1;
                     let newTail = take(j,t);
                     Cons(h,newTail)
    }
  }
};
