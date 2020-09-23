/*@ val maxInt : forall (p : int => bool). x:int[v|p v] => y:int[v|p v] => int[v|p v] */
let maxInt = (x, y) => {
  let b = x < y;
  if (b){
    y
  } else {
    x
  }
};

/*@ val test1 : a:int[v|0 < v] => b:int[v|0 < v] => int[v|0 < v] */
let test1 = (a, b) => {
  maxInt(a, b)
};

/*@ val test2 : a:int[v|v < 0] => b:int[v|v < 0] => int[v|v < 0] */
let test2 = (a, b) => {
  maxInt(a, b)
};