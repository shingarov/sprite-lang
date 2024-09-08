type void() =
  | Nil
  ;

/*@ val cassert : bool[b|b] => int */
let cassert = (b) => { 0 };

/*@ val main : int => int */
let main = (x) => {
  let a = Nil;
  let b = Nil;
  let ok = a == b;
  cassert(ok)
};

