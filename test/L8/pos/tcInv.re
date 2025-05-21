/*@ measure len : f('a) => int */

type f('a) [v|len(v) >= 0] =
  | Nil                      => [v| v = Nil && len v = 0] 
  ;

/*@ val main : int => int */
let main = (arg) => {
  let xs = Nil;
  0
};
