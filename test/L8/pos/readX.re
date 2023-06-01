/*@ measure len : heap('ptr) => int */

type heap('ptr) [v|len(v) >= 0] =
  | Emp                                 => [v| v = Emp && len v = 0] 
  | Disj (p:'ptr, xs:heap('ptr), b:int) => [v| v = Disj(p, xs, b) && len v = 1 + len(xs)]
  ;


/*@ val own : 'ptr => h:heap('ptr) => bool / len(h) */
let rec own = (p,h) => {
  switch (h) {
    | Emp        => false
    | Disj(q,hh,b) => let found = p==q;
                      if (found) {
                        true
                      } else {
                        own(p,hh)
                      }
  }
};

