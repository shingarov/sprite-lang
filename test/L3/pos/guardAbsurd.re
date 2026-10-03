/*@ val cassert : bool[b|b] => int */
let cassert = (b) => { 
  0 
};

/*@ val main : int[v|false] => int */
let main = (x) => {
  let ok = 10 < 0;
  cassert(ok)
};
