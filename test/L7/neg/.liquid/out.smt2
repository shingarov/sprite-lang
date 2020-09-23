(set-option :auto-config false)
(set-option :model true)
(set-option :model.partial false)

(set-option :smt.mbqi false)

(define-sort Str () Int)
(declare-fun strLen (Str) Int)
(declare-fun subString (Str Int Int) Str)
(declare-fun concatString (Str Str) Str)
(define-sort Elt () Int)
(define-sort LSet () (Array Elt Bool))
(define-fun smt_set_emp () LSet ((as const LSet) false))
(define-fun smt_set_mem ((x Elt) (s LSet)) Bool (select s x))
(define-fun smt_set_add ((s LSet) (x Elt)) LSet (store s x true))
(define-fun smt_set_cup ((s1 LSet) (s2 LSet)) LSet ((_ map or) s1 s2))
(define-fun smt_set_cap ((s1 LSet) (s2 LSet)) LSet ((_ map and) s1 s2))
(define-fun smt_set_com ((s LSet)) LSet ((_ map not) s))
(define-fun smt_set_dif ((s1 LSet) (s2 LSet)) LSet (smt_set_cap s1 (smt_set_com s2)))
(define-fun smt_set_sub ((s1 LSet) (s2 LSet)) Bool (= smt_set_emp (smt_set_dif s1 s2)))
(define-sort Map () (Array Elt Elt))
(define-fun smt_map_sel ((m Map) (k Elt)) Elt (select m k))
(define-fun smt_map_sto ((m Map) (k Elt) (v Elt)) Map (store m k v))
(define-fun smt_map_cup ((m1 Map) (m2 Map)) Map ((_ map (+ (Elt Elt) Elt)) m1 m2))
(define-fun smt_map_def ((v Elt)) Map ((as const (Map)) v))
(define-fun bool_to_int ((b Bool)) Int (ite b 1 0))
(define-fun Z3_OP_MUL ((x Int) (y Int)) Int (* x y))
(define-fun Z3_OP_DIV ((x Int) (y Int)) Int (div x y))
(declare-datatypes ((list 1)) ((par (T0) (Nil (Cons (Cons$35$$35$0 T0) (Cons$35$$35$1 (list T0)))))))
(declare-fun VV$35$7 () Int)
(declare-fun nnf_arg$35$$35$k_$35$$35$1$35$$35$4 () Int)
(declare-fun cast_as_int () Int)
(declare-fun v$35$1 () (list Int))
(declare-fun papp3 () Int)
(declare-fun VV$35$2$35$$35$3 () (list Int))
(declare-fun VV$35$1 () Int)
(declare-fun nnf_arg$35$$35$k_$35$$35$1$35$$35$2 () (list Int))
(declare-fun VV$35$6 () (list Int))
(declare-fun VV () Int)
(declare-fun xs () (list Int))
(declare-fun nnf_arg$35$$35$k_$35$$35$1$35$$35$3 () (list Int))
(declare-fun nnf_arg$35$$35$k_$35$$35$3$35$$35$1 () (list Int))
(declare-fun papp2 () Int)
(declare-fun VV$35$5 () Int)
(declare-fun nnf_arg$35$$35$k_$35$$35$3$35$$35$4 () Int)
(declare-fun v$35$3 () (list Int))
(declare-fun xs$35$1 () (list Int))
(declare-fun t () (list Int))
(declare-fun nnf_arg$35$$35$k_$35$$35$3$35$$35$5 () (list Int))
(declare-fun v$35$$35$1 () (list Int))
(declare-fun len () Int)
(declare-fun v$35$3$35$$35$9 () (list Int))
(declare-fun xs$35$2 () (list Int))
(declare-fun ys () (list Int))
(declare-fun h () Int)
(declare-fun papp1 () Int)
(declare-fun x () Int)
(declare-fun VV$35$3 () Int)
(declare-fun nnf_arg$35$$35$k_$35$$35$3$35$$35$2 () (list Int))
(declare-fun rest () (list Int))
(declare-fun VV$35$6$35$$35$5 () (list Int))
(declare-fun xs$35$3 () (list Int))
(declare-fun elts () Int)
(declare-fun v$35$1$35$$35$2 () (list Int))
(declare-fun v () (list Int))
(declare-fun nnf_arg$35$$35$k_$35$$35$3$35$$35$3 () (list Int))
(declare-fun VV$35$2 () (list Int))
(declare-fun nnf_arg$35$$35$k_$35$$35$1$35$$35$1 () (list Int))
(declare-fun apply$35$$35$25 (Int (_ BitVec 32)) Bool)
(declare-fun apply$35$$35$20 (Int LSet) Str)
(declare-fun apply$35$$35$13 (Int Str) Bool)
(declare-fun apply$35$$35$32 (Int (list Int)) Str)
(declare-fun apply$35$$35$8 (Int Bool) Str)
(declare-fun apply$35$$35$33 (Int (list Int)) LSet)
(declare-fun apply$35$$35$16 (Int Str) (_ BitVec 32))
(declare-fun apply$35$$35$9 (Int Bool) LSet)
(declare-fun apply$35$$35$21 (Int LSet) LSet)
(declare-fun apply$35$$35$35 (Int (list Int)) (list Int))
(declare-fun apply$35$$35$28 (Int (_ BitVec 32)) (_ BitVec 32))
(declare-fun apply$35$$35$0 (Int Int) Int)
(declare-fun apply$35$$35$11 (Int Bool) (list Int))
(declare-fun apply$35$$35$12 (Int Str) Int)
(declare-fun apply$35$$35$1 (Int Int) Bool)
(declare-fun apply$35$$35$5 (Int Int) (list Int))
(declare-fun apply$35$$35$3 (Int Int) LSet)
(declare-fun apply$35$$35$18 (Int LSet) Int)
(declare-fun apply$35$$35$10 (Int Bool) (_ BitVec 32))
(declare-fun apply$35$$35$15 (Int Str) LSet)
(declare-fun apply$35$$35$34 (Int (list Int)) (_ BitVec 32))
(declare-fun apply$35$$35$22 (Int LSet) (_ BitVec 32))
(declare-fun apply$35$$35$27 (Int (_ BitVec 32)) LSet)
(declare-fun apply$35$$35$19 (Int LSet) Bool)
(declare-fun apply$35$$35$23 (Int LSet) (list Int))
(declare-fun apply$35$$35$26 (Int (_ BitVec 32)) Str)
(declare-fun apply$35$$35$14 (Int Str) Str)
(declare-fun apply$35$$35$31 (Int (list Int)) Bool)
(declare-fun apply$35$$35$7 (Int Bool) Bool)
(declare-fun apply$35$$35$2 (Int Int) Str)
(declare-fun apply$35$$35$29 (Int (_ BitVec 32)) (list Int))
(declare-fun apply$35$$35$24 (Int (_ BitVec 32)) Int)
(declare-fun apply$35$$35$4 (Int Int) (_ BitVec 32))
(declare-fun apply$35$$35$17 (Int Str) (list Int))
(declare-fun apply$35$$35$6 (Int Bool) Int)
(declare-fun apply$35$$35$30 (Int (list Int)) Int)
(declare-fun coerce$35$$35$25 ((_ BitVec 32)) Bool)
(declare-fun coerce$35$$35$20 (LSet) Str)
(declare-fun coerce$35$$35$13 (Str) Bool)
(declare-fun coerce$35$$35$32 ((list Int)) Str)
(declare-fun coerce$35$$35$8 (Bool) Str)
(declare-fun coerce$35$$35$33 ((list Int)) LSet)
(declare-fun coerce$35$$35$16 (Str) (_ BitVec 32))
(declare-fun coerce$35$$35$9 (Bool) LSet)
(declare-fun coerce$35$$35$21 (LSet) LSet)
(declare-fun coerce$35$$35$35 ((list Int)) (list Int))
(declare-fun coerce$35$$35$28 ((_ BitVec 32)) (_ BitVec 32))
(declare-fun coerce$35$$35$0 (Int) Int)
(declare-fun coerce$35$$35$11 (Bool) (list Int))
(declare-fun coerce$35$$35$12 (Str) Int)
(declare-fun coerce$35$$35$1 (Int) Bool)
(declare-fun coerce$35$$35$5 (Int) (list Int))
(declare-fun coerce$35$$35$3 (Int) LSet)
(declare-fun coerce$35$$35$18 (LSet) Int)
(declare-fun coerce$35$$35$10 (Bool) (_ BitVec 32))
(declare-fun coerce$35$$35$15 (Str) LSet)
(declare-fun coerce$35$$35$34 ((list Int)) (_ BitVec 32))
(declare-fun coerce$35$$35$22 (LSet) (_ BitVec 32))
(declare-fun coerce$35$$35$27 ((_ BitVec 32)) LSet)
(declare-fun coerce$35$$35$19 (LSet) Bool)
(declare-fun coerce$35$$35$23 (LSet) (list Int))
(declare-fun coerce$35$$35$26 ((_ BitVec 32)) Str)
(declare-fun coerce$35$$35$14 (Str) Str)
(declare-fun coerce$35$$35$31 ((list Int)) Bool)
(declare-fun coerce$35$$35$7 (Bool) Bool)
(declare-fun coerce$35$$35$2 (Int) Str)
(declare-fun coerce$35$$35$29 ((_ BitVec 32)) (list Int))
(declare-fun coerce$35$$35$24 ((_ BitVec 32)) Int)
(declare-fun coerce$35$$35$4 (Int) (_ BitVec 32))
(declare-fun coerce$35$$35$17 (Str) (list Int))
(declare-fun coerce$35$$35$6 (Bo(assert (and (not condm) (= m1 (- m 1)) (and (= v$35$5$35$$35$5 (- m 1)) (= v$35$5$35$$35$5 m1)) (<= 0 n) condn (<= 0 m) (and (= v$35$5 (- m 1)) (= v$35$5 m1)) (= condm (= m 0)) (= condn (= n 0))))
(push 1)
(assert (not (<= 0 v$35$5$35$$35$5)))
(check-sat)
; SMT Says: Unsat
(pop 1)
(pop 1)
(push 1)
(assert (and (not condm) (= m1 (- m 1)) (<= 0 n) condn (= VV$35$4$35$$35$6 1) ((declare-fun smt_lambda$35$$35$16 (Str (_ BitVec 32)) Int)
(declare-fun smt_lambda$35$$35$9 (Bool LSet) Int)
(declare-fun smt_lambda$35$$35$21 (LSet LSet) Int)
(declare-fun smt_lambda$35$$35$35 ((list Int) (list Int)) Int)
(declare-fun smt_lambda$35$$35$28 ((_ BitVec 32) (_ BitVec 32)) Int)
(declare-fun smt_lambda$35$$35$0 (Int Int) Int)
(declare-fun smt_lambda$35$$35$11 (Bool (list Int)) Int)
(declare-fun smt_lambda$35$$35$12 (Str Int) Int)
(declare-fun smt_lambda$35$$35$1 (Int Bool) Int)
(declare-fun smt_lambda$35$$35$5 (Int (list Int)) Int)
(declare-fun smt_lambda$35$$35$3 (Int LSet) Int)
(declare-fun smt_lambda$35$$35$18 (LSet Int) Int)
(declare-fun smt_lambda$35$$35$10 (Bool (_ BitVec 32)) Int)
(declare-fun smt_lambda$35$$35$15 (Str LSet) Int)
(declare-fun smt_lambda$35$$35$34 ((list Int) (_ BitVec 32)) Int)
(declare-fun smt_lambda$35$$35$22 (LSet (_ BitVec 32)) Int)
(declare-fun smt_lambda$35$$35$27 ((_ BitVec 32) LSet) Int)
(declare-fun smt_lambda$35$$35$19 (LSet Bool) Int)
(declare-fun smt_lambda$35$$35$23 (LSet (list Int)) Int)
(declare-fun smt_lambda$35$$35$26 ((_ BitVec 32) Str) Int)
(declare-fun smt_lambda$35$$35$14 (Str Str) Int)
(declare-fun smt_lambda$35$$35$31 ((list Int) Bool) Int)
(declare-fun smt_lambda$35$$35$7 (Bool Bool) Int)
(declare-fun smt_lambda$35$$35$2 (Int Str) Int)
(declare-fun smt_lambda$35$$35$29 ((_ BitVec 32) (list Int)) Int)
(declare-fun smt_lambda$35$$35$24 ((_ BitVec 32) Int) Int)
(declare-fun smt_lambda$35$$35$4 (Int (_ BitVec 32)) Int)
(declare-fun smt_lambda$35$$35$17 (Str (list Int)) Int)
(declare-fun smt_lambda$35$$35$6 (Bool Int) Int)
(declare-fun smt_lambda$35$$35$30 ((list Int) Int) Int)
(declare-fun lam_arg$35$$35$1$35$$35$0 () Int)
(declare-fun lam_arg$35$$35$2$35$$35$0 () Int)
(declare-fun lam_arg$35$$35$3$35$; SMT Says: Unsat
(pop 1)
(pop 1)
(push 1)
(assert (and (not condm) (= VV$35$7 r) (= m1 (- m 1)) (<= 0 n) (<= 0 m) (= condm (= m 0)) (not condn) (= VV$35$7$35$$35$12 r) (= condn (= n 0)) (= n1 (- n 1))))
(push 1)
(assert (not (<= 0 VV$35$7$35$$35$12)))
(check-sat)
; SMT Says: Sat
(pop 1)
(pop 1)
(push 1)
(assert (and (not condm) (= VV$35$7 r) (= m1 (- m 1)) (<= 0 n) (<= 0 m) (= condm (= m 0)) (not condn) (= condn (= n 0)) (= n1 (- n 1)) (= VV$35$7$35$$35$13 r)))
(push 1)
(assert (not (and (<= 0 m1) (or  (< m1 m) (and (= m1 m) (and (<= 0 VV$35$7$35$$35$13) (< VV$35$7$35$$35$13 n)))))))
(check-sat)
; SMT Says: Unsat
(pop 1)
(pop 1)
(exit)
fun lam_arg$35$$35$3$35$$35$18 () LSet)
(declare-fun lam_arg$35$$35$4$35$$35$18 () LSet)
(declare-fun lam_arg$35$$35$5$35$$35$18 () LSet)
(declare-fun lam_arg$35$$35$6$35$$35$18 () LSet)
(declare-fun lam_arg$35$$35$7$35$$35$18 () LSet)
(declare-fun lam_arg$35$$35$1$35$$35$24 () (_ BitVec 32))
(declare-fun lam_arg$35$$35$2$35$$35$24 () (_ BitVec 32))
(declare-fun lam_arg$35$$35$3$35$$35$24 () (_ BitVec 32))
(declare-fun lam_arg$35$$35$4$35$$35$24 () (_ BitVec 32))
(declare-fun lam_arg$35$$35$5$35$$35$24 () (_ BitVec 32))
(declare-fun lam_arg$35$$35$6$35$$35$24 () (_ BitVec 32))
(declare-fun lam_arg$35$$35$7$35$$35$24 () (_ BitVec 32))
(declare-fun lam_arg$35$$35$1$35$$35$6 () Bool)
(declare-fun lam_arg$35$$35$2$35$$35$6 () Bool)
(declare-fun lam_arg$35$$35$3$35$$35$6 () Bool)
(declare-fun lam_arg$35$$35$4$35$$35$6 () Bool)
(declare-fun lam_arg$35$$35$5$35$$35$6 () Bool)
(declare-fun lam_arg$35$$35$6$35$$35$6 () Bool)
(declare-fun lam_arg$35$$35$7$35$$35$6 () Bool)
(declare-fun lam_arg$35$$35$1$35$$35$30 () (list Int))
(declare-fun lam_arg$35$$35$2$35$$35$30 () (list Int))
(declare-fun lam_arg$35$$35$3$35$$35$30 () (list Int))
(declare-fun lam_arg$35$$35$4$35$$35$30 () (list Int))
(declare-fun lam_arg$35$$35$5$35$$35$30 () (list Int))
(declare-fun lam_arg$35$$35$6$35$$35$30 () (list Int))
(declare-fun lam_arg$35$$35$7$35$$35$30 () (list Int))
(push 1)
(assert (and (and (= v$35$$35$1 (as Nil (list Int))) (= (apply$35$$35$30 (as len Int) v$35$$35$1) 0) (= (apply$35$$35$33 (as elts Int) v$35$$35$1) smt_set_emp)) (and (= v (as Nil (list Int))) (= (apply$35$$35$30 (as len Int) v) 0) (= (apply$35$$35$33 (as elts Int) v) smt_set_emp))))
(push 1)
(assert (not (>= (apply$35$$35$30 (as len Int) v$35$$35$1) 0)))
(check-sat)
; SMT Says: Unsat
(pop 1)
(pop 1)
(push 1)
(assert (and (and (= v$35$1$35$$35$2 ((as Cons (list Int)) x xs)) (= (apply$35$$35$30 (as len Int) v$35$1$35$$35$2) (+ 1 (apply$35$$35$30 (as len Int) xs))) (= (apply$35$$35$33 (as elts Int) v$35$1$35$$35$2) (smt_set_add (apply$35$$35$33 (as elts Int) xs) x))) (>= (apply$35$$35$30 (as len Int) xs) 0) (and (= v$35$1 ((as Cons (list Int)) x xs)) (= (apply$35$$35$30 (as len Int) v$35$1) (+ 1 (apply$35$$35$30 (as len Int) xs))) (= (apply$35$$35$33 (as elts Int) v$35$1) (smt_set_add (apply$35$$35$33 (as elts Int) xs) x)))))
(push 1)
(assert (not (>= (apply$35$$35$30 (as len Int) v$35$1$35$$35$2) 0)))
(check-sat)
; SMT Says: Unsat
(pop 1)
(pop 1)
(push 1)
(assert (and (>= (apply$35$$35$30 (as len Int) ys) 0) (and (= xs$35$2 (as Nil (list Int))) (= (apply$35$$35$30 (as len Int) xs$35$2) 0) (= (apply$35$$35$33 (as elts Int) xs$35$2) smt_set_emp) (>= (apply$35$$35$30 (as len Int) xs$35$2) 0)) (and (= VV$35$2 ys) (>= (apply$35$$35$30 (as len Int) VV$35$2) 0)) (and (= VV$35$2$35$$35$3 ys) (>= (apply$35$$35$30 (as len Int) VV$35$2$35$$35$3) 0)) (>= (apply$35$$35$30 (as len Int) xs$35$1) 0)))
(push 1)
(assert (not (= (apply$35$$35$33 (as elts Int) VV$35$2$35$$35$3) (smt_set_cup (apply$35$$35$33 (as elts Int) xs$35$2) (apply$35$$35$33 (as elts Int) ys)))))
(check-sat)
; SMT Says: Unsat
(pop 1)
(pop 1)
(push 1)
(assert (and (>= (apply$35$$35$30 (as len Int) ys) 0) (>= (apply$35$$35$30 (as len Int) t) 0) (and (= VV$35$6$35$$35$5 ys) (>= (apply$35$$35$30 (as len Int) VV$35$6$35$$35$5) 0)) (and (= xs$35$3 ((as Cons (list Int)) h t)) (= (apply$35$$35$30 (as len Int) xs$35$3) (+ 1 (apply$35$$35$30 (as len Int) t))) (= (apply$35$$35$33 (as elts Int) xs$35$3) (smt_set_add (apply$35$$35$33 (as elts Int) t) h)) (>= (apply$35$$35$30 (as len Int) xs$35$3) 0)) (and (= VV$35$6 ys) (>= (apply$35$$35$30 (as len Int) VV$35$6) 0)) (>= (apply$35$$35$30 (as len Int) xs$35$1) 0)))
(push 1)
(assert (not (and (<= 0 (apply$35$$35$30 (as len Int) VV$35$6$35$$35$5)) (< (apply$35$$35$30 (as len Int) VV$35$6$35$$35$5) (apply$35$$35$30 (as len Int) ys)))))
(check-sat)
; SMT Says: Sat
(pop 1)
(pop 1)
(push 1)
(assert (and (>= (apply$35$$35$30 (as len Int) ys) 0) (>= (apply$35$$35$30 (as len Int) t) 0) (and (= xs$35$3 ((as Cons (list Int)) h t)) (= (apply$35$$35$30 (as len Int) xs$35$3) (+ 1 (apply$35$$35$30 (as len Int) t))) (= (apply$35$$35$33 (as elts Int) xs$35$3) (smt_set_add (apply$35$$35$33 (as elts Int) t) h)) (>= (apply$35$$35$30 (as len Int) xs$35$3) 0)) (and (= v$35$3$35$$35$9 ((as Cons (list Int)) h rest)) (= (apply$35$$35$30 (as len Int) v$35$3$35$$35$9) (+ 1 (apply$35$$35$30 (as len Int) rest))) (= (apply$35$$35$33 (as elts Int) v$35$3$35$$35$9) (smt_set_add (apply$35$$35$33 (as elts Int) rest) h)) (>= (apply$35$$35$30 (as len Int) v$35$3$35$$35$9) 0)) (and (= (apply$35$$35$33 (as elts Int) rest) (smt_set_cup (apply$35$$35$33 (as elts Int) t) (apply$35$$35$33 (as elts Int) ys))) (>= (apply$35$$35$30 (as len Int) rest) 0)) (>= (apply$35$$35$30 (as len Int) xs$35$1) 0) (and (= v$35$3 ((as Cons (list Int)) h rest)) (= (apply$35$$35$30 (as len Int) v$35$3) (+ 1 (apply$35$$35$30 (as len Int) rest))) (= (apply$35$$35$33 (as elts Int) v$35$3) (smt_set_add (apply$35$$35$33 (as elts Int) rest) h)) (>= (apply$35$$35$30 (as len Int) v$35$3) 0))))
(push 1)
(assert (not (= (apply$35$$35$33 (as elts Int) v$35$3$35$$35$9) (smt_set_cup (apply$35$$35$33 (as elts Int) xs$35$3) (apply$35$$35$33 (as elts Int) ys)))))
(check-sat)
; SMT Says: Unsat
(pop 1)
(pop 1)
(exit)
