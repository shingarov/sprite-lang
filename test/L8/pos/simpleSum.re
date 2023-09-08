/*@ reflect sum : n:int => int / n */
let rec sum = (n) => {  
  let base = n <= 0;
  if (base) {
    0
  } else {
    let n1 = n - 1;
    let t1 = sum(n1);
    n + t1
  }
};

/*@ val cassert : bool[b|b] => int */
let cassert = (b) => { 0 };

/*@ val sum_3_eq_6 : int => int */
let sum_3_eq_6 = (x) => {
  let three = 3;
  let thesum = sum(three);
  let six = 6;
  let ok = thesum == six;
  cassert(ok)
};
