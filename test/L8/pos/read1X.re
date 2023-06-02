/*@ measure len : heap('ptr) => int */

type heap('ptr) [v|len(v) >= 0] =
  | Emp                                 => [v| v = Emp && len v = 0] 
  | Disj (p:'ptr, xs:heap('ptr[q| q != p]), b:int) => [v| v = Disj(p, xs, b) && len v = 1 + len(xs)]
  ;


/*@ reflect own : 'ptr => h:heap('ptr) => bool / len(h) */
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


/*@ reflect read : p:'ptr => h:heap('ptr)[v| own(p,v)] => int / len(h) */
let rec read = (p,h) => {
  switch (h) {
    | Emp => impossible(0)
    | Disj(q,hh,b) => let found = p==q;
                      if (found) {
                        b
                      } else {
                        read(p,hh)
                      }
  }
};


/*@ val checkRead1 : int => int[v| 0 <= v ] */
let checkRead1 = (x) => {
  let p = 1024;
  let vvv = 42;
  let h0 = Emp;
  let h1 = Disj(p,h0,vvv);
  let r = read(p,h1);
  20
};
