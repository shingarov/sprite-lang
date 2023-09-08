/*@ val cassert : bool[b|b] => int */
let cassert = (b) => { 0 };

/*@ reflect iconst : int => int / 0 */
let rec iconst = (c) => { c };

/*@ reflect icmpeq : int => int => int / 0 */
let rec icmpeq = (e1, e2) => {
  let eq = e1==e2;
  if (eq) {
    1
  } else {
    0
  }
};

/*@ reflect ificmpeq : int => int => int => int => int / 0 */
let rec ificmpeq = (e1, e2, l, c) => {
  let eq = e1==e2;
  if (eq) {
    l
  } else {
    c
  }
};


/*@ reflect complex : int => int => int => int => int / 0 */
let rec complex = (e1, e2, l, c) => { 
  let vr1 = e1;
  let vr2 = e2;
  let vr3 = icmpeq(vr1, vr2);
  let one = 1;
  let vr4 = iconst(one);
  let vr5 = ificmpeq(vr3, vr4, l, c);
  vr5
};

/*@ reflect simplified : int => int => int => int => int / 0 */
let rec simplified = (e1, e2, l, c) => { 
  let vr1 = e1;
  let vr2 = e2;
  let vr3 = ificmpeq(vr1, vr2, l, c);
  vr3
};

/*@ val main : int => int => int */
let main = (x, y) => {
  let fortyTwo = 42;
  let fiftyTwo = 52;
  let vr1 = complex(x,y,fortyTwo,fiftyTwo);
  let vr2 = simplified(x,y,fortyTwo,fiftyTwo);
  let ok = vr1 == vr2;
  cassert(ok)
};



/*@ reflect sum : n:int => int / 0 */
let rec sum = (n) => {  
  6
};

/*@ val sum_3_eq_6 : int => int */
let sum_3_eq_6 = (x) => {
  let three = 3;
  let thesum = sum(three);
  let six = 6;
  let ok = thesum == six;
  cassert(ok)
};
