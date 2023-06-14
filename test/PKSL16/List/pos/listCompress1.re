/*@ measure elts : list('a) => Set_Set('a) */
/*@ measure len : list('a) => int */

type plist('a)(p : 'a => plist('a)(p) => bool) [v| len(v) >= 0] =
  | Nil                                                       => [v| v = Nil && elts(v) = Set_empty(0) && len(v) = 0]
  | Cons(x:'a, xs:plist('a)((x:'a, v:plist('a,p)) => p x v))  => [v| v = Cons(x,xs) && elts(v) = Set_add(elts(xs), x) && len(v) = 1 + len(xs)]
  ;

type list('a) = plist('a)((xx,ll) => true)

/*@ val partition : p:'a => xs:list('a) => pair(list('a[v|v<=p]), list('a[v|v>p]))[v| len(fst(v)) + len(snd(v)) == len(xs) && Set_cup(elts(fst(v)), elts(snd(v))) == elts(xs) ]  / len(xs) */
let rec partition = (p, xs) => {
  switch (xs) {
    | Nil        => let n1 = Nil;
                    let n2 = Nil;
                    MkPair(n1, n2)
    | Cons(h, t) => let rest = partition(p, t);
                    switch (rest) {
                      | MkPair(l,r) => let smaller = h<=p;
                                       if (smaller) {
                                         let newLeft = Cons(h, l);
                                         MkPair(newLeft, r)
                                       } else {
                                         let newRight = Cons(h, r);
                                         MkPair(l, newRight)
                                       }
                    }
  }
};
