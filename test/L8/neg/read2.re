/*@ measure len : heap('ptr) => int */

type heap('ptr) [v|len(v) >= 0] =
  | Emp                                 => [v| v = Emp && len v = 0] 
  | Disj (p:'ptr, xs:heap('ptr[q| q != p]), b:int[bb| 0<bb ]) => [v| v = Disj(p, xs, b) && len v = 1 + len(xs)]
  ;


/*@ val cassert : bool[b|b] => int */
let cassert = (b) => {
  0
};


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


/*@ reflect read : p:'ptr => h:heap('ptr)[v| own(p,v)] => int[nz| nz != 0] / len(h) */
let rec read = (p,h) => {
  switch (h) {
    | Emp => 0
    | Disj(q,hh,b) => let found = p==q;
                      if (found) {
                        b
                      } else {
                        read(p,hh)
                      }
  }
};


/*@ val checkRead2 : int => int */
let checkRead2 = (x) => {
  let p1 = 1024;
  let p2 = 1024;
  let vvv = 42;
  let www = 43;
  let h0 = Emp;
  let h1 = Disj(p1,h0,vvv);
  let h2 = Disj(p2,h1,www);

  let r1 = read(p1,h2);
  let ok1 = r1==42;
  let zz1 = cassert(ok1);

  let r2 = read(p2,h2);
  let ok2 = r2==43;
  let zz2 = cassert(ok2);

  1
};
