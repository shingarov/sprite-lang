/*@ val f : m:int => int[v| v == m] */
let f = (m) => { m };

/*@ reflect g : int => int / 0 */
let rec g = (n) => { n };

/*@ val check : x:int => int[w| w == x] */
let check = (x) => {
  let y = g(x);
  f(y)
};

