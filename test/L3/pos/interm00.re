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
