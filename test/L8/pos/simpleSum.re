/*@ reflect sum : n:int => int / 0 */
let rec sum = (n) => {  
  6
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
