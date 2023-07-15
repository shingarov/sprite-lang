/*@ measure encoding : instruction => int */

type instruction = 
  | A => [v| encoding v = 0]
  | B => [v| encoding v = 1]
  ;

/*@ val cassert : bool[b|b] => int */
let cassert = (b) => { 
  0 
};

/*@ val encode : instruction => int[?] */
let encode = (insn) => { 
  switch (insn) {
    | A => 0
    | B => 2
  }
};

/*@ val main : insn:instruction => int */
let main = (insn) => {
  let e = encode(insn);
  let isZero = e == 0;
  let isOne = e == 1;
  let good = isZero || isOne;
  cassert(good)
};
