

/*@ val compose : forall (p : 'b => 'c => bool).
                  forall (q : 'a => 'b => bool).
                  forall (r : 'a => 'c => bool).
                  (x:'a => w:'b[v|q x v] => z:'c[v|p w v] => bool[v|r x z])
               => (y:'b => 'c[v | p y v])
               => (z:'a => 'b[v | q z v])
               =>  x:'a
               => 'c[v | r x v]                */
let compose = (cha, f, g, x) => {
  let t1 = g(x);
  let t2 = f(t1);
  let t3 = cha(x, t1, t2);
  t2
};

/*@ val incr : n:int => int[v | v == (n + 1)] */
let incr = (n) => {
  n + 1
};

/*@ val cha0 : x:int => y:int[?] => z:int[?] => bool[?] */
let cha0 = (x, y, z) => { true };

/*@ val add2 : n:int => int[v | v == (n + 2)] */
let add2 = compose(cha0, incr, incr);
