/*@ measure size : tree('a) => int */

type tree('a) [v| size(v) >= 0] =
  | Nil                                 => [v| v = Nil && size(v) = 0]
  | Cons (x:'a, l:tree('a), r:tree('a)) => [v| v = Cons(x,l,r) && size(v) = 1 + size(l) + size(r) ]
  ;

/*@ val countNodes : t:tree('a) => int[v|v == size(t)]  / size(t) */
let rec countNodes = (t) => {
  switch (t) {
    | Nil         => 0
    | Cons(x,l,r) => let countLeft = countNodes(l);
                     let countRight = countNodes(r);
                     let together = countLeft + countRight;
                     together + 1
  }
};
