
type list('a) =
  | Nil
  | Cons (x:'a, xs:list('a))
  ;


/*@ val foldr : forall (p: list('a) => 'b => bool).
                nil:list('a)[v| v = (Nil)] =>
                (xs:list('a) => x:'a => acc:'b[v|p xs v] => cns:list('a)[v| v = (Cons x xs)] => 'b[v|p cns v])  =>
                seed:'b[v|p nil v]                                                =>
                ys:list('a)                                                         =>
                'b[v|p ys v]                  */
let rec foldr = (n, op, seed, ys) => {
  switch (ys) {
  | Nil        => seed
  | Cons(h, t) => let nil = Nil;
                  let nextb = foldr(nil, op, seed, t);
                  let cns = Cons(h,t);
                  op(t, h, nextb, cns)
  }
};
