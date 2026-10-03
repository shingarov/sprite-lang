/*@ val cassert : bool[b|b] => int */
let cassert = (b) => { 
  0 
};

/*@ val addFive : int[?] => int */
let addFive = (x) => {
  let res = x + 5;
  let ok = res > 10;
  let cass = cassert(ok);
  res
};

/*@ val main : int => int */
let main = (x) => {
  let a   = 100;
  let y = addFive(a);
  let b   = 6;
  let z = addFive(b);
  z
};
