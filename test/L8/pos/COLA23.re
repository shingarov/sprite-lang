/*@ val static_assert : bool[b|b] => int */
let static_assert = (b) => { 0 };

/*@ reflect iconst : int => int / 0 */
let rec iconst = (const) => {
  const
};

/*@ reflect icmpeq : int => int => int / 0 */
let rec icmpeq = (e1, e2) => {
  let eq = e1==e2;
  if (eq) {
    1
  } else {
    0
  }
};

/*@ reflect icmp : int => int => int / 0 */
let rec icmp = (e1, e2) => {
  let eq = e1==e2;
  let le = e1<e2;
  let minusOne = 0 - 1; /* Sprite does not support literal -1 */
  if (eq) {
    0
  } else {
    if (le) {
      minusOne
    } else {
      1
    }
  }
};

/*@ reflect ificmpne : int => int => 'c => 'c => 'c / 0 */
let rec ificmpne = (e1, e2, cont, jump) => {
  let eq = e1==e2;
  if (eq) {
    cont
  } else {
    jump
  }
};


/*@ reflect original : int => int => 'c => 'c => 'c / 0 */
let rec original = (a, b, l0, l1) => {
  let vr1 = a;
  let vr2 = b;
  let vr3 = icmpeq(vr1, vr2);
  let one = 1;
  let vr4 = iconst(one);
  let next = ificmpne(vr3, vr4, l0, l1);
  next
};

/*@ reflect simplified : int => int => 'c => 'c => 'c / 0 */
let rec simplified = (a, b, l0, l1) => {
  let vr1 = a;
  let vr2 = b;
  let next = ificmpne(vr1, vr2, l0, l1);
  next
};

/*@ val justify : int => int => 'c => 'c => int */
let justify = (a, b, l0, l1) => {
  let vr1 = original  (a, b, l0, l1);
  let vr2 = simplified(a, b, l0, l1);
  let ok = vr1 == vr2;
  static_assert(ok)
};