/*@ val f : c1:int => int[v| v == c1] */
let f = (c1) => { c1 };

/*@ reflect g : int => int / 0 */
let rec g = (n) => { f(n) };

/*@ val verify : int => int */
let verify = (x) => { g(x) };

