

/*@ val compose : forall (p : 'b => 'c => bool).
                  forall (q : 'a => 'b => bool).
                  forall (r : 'a => 'c => bool).
                  (x:'a => w:'b => z:'c => int[v|q x w] => int[v|p w z] => Bool[v|r x z])
               => (y:'b => 'c[v | p y v])
               => (z:'a => 'b[v | q z v])
               =>  x:'a
               => 'c[v | r x v]                */
let compose = (cha, f, g, x) => {
  let t1 = g(x);
  let t2 = f(t1);
  let t3 = cha(x, t1, t2, 0, 0);
  t2
};


/*@ val incr : n:int => int[v | v == (n + 1)] */
let incr = (n) => {
  n + 1
};


/*@ val cha0 : x:'a => w:'b => z:'c => int[v|q x w] => int[v|p w z] => Bool[v|r x z] */
let cha0 = (x, w, z, i1, i2) => { true };


/*@ val ex2 : n:int => int[v | v == (n + 2)] */
let ex2 = compose(cha0, incr, incr);
