/*@ measure elts : list('a) => Set_Set('a) */

/*@ measure len : list('a) => int */


type list('a) [v| len(v) >= 0] =
  | Nil                      => [v| v = Nil && elts(v) = Set_empty(0) && len(v) = 0]
  | Cons (x:'a, xs:list('a)) => [v| v = Cons(x,xs) && elts(v) = Set_add(elts(xs), x) && len(v) = 1 + len(xs)]
  ;

/*@ val delete : x:'a => xs:list('a) => list('a)[v|elts(v) = Set_dif(elts(xs), Set_add(Set_empty(0),x))] / len(xs) */
let rec delete = (x, xs) => {
  switch (xs) {
    | Nil => Nil
    | Cons(h,t) => let deleteHead = x==h;
                   if (deleteHead) {
                     delete(x,t)
                   } else {
                     let newTail = delete(x,t);
                     Cons(h,newTail)
                   }
  }
};
