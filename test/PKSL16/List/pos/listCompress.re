
type plist('a)(p : 'a => plist('a) => bool) =
  | Nil
  | Cons(x:'a, xs:plist('a)((x:'a, v:plist('a)) => p x v))
  ;


/*@ val compress : xs:int => int */
let compress = (xs) => {
  xs
};
