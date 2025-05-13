
type coord = 
  | C (x:int, y:int)
  ;

/*@ val cassert : bool[b|b] => int */
let cassert = (b) => { 
  0 
};

/*@ val check : m:int => int */
let check = (m) => {
    let p = C(42, 43);
    switch (p){
      | C(px, py) => let ok = px == 42;
                     cassert(ok)
    }
};
