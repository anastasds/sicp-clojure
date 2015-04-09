;; 1.1 The Elements of Programming
;; 1.1.1 Expressions
486 ; -> 486

(+ 137 349) ; -> 486
(- 1000 334) ; -> 666
(* 5 99) ; -> 495
(/ 10 5) ; -> 2
(+ 2.7 10) ; -> 12.7

(+ 21 35 12 7) ; -> 75
(* 25 4 12) ; -> 1200

(+ (* 3 5) (- 10 6)) ; -> 19

(+ (* 3 (+ (* 2 4) (+ 3 5))) (+ (- 10 7) 6)) ; -> 57


;; 1.1.2 Naming and the Environment
(def size 2)
size ; -> 2
(* 5 size) ; -> 10

(def pi 3.14159)
(def radius 10)
(* pi (* radius radius)) ; -> 314.159
(def circumference (* 2 pi radius))
circumference ; -> 62.8318


;; 1.1.3 Evaluating Combinations
(* (+ 2 (* 4 6))
   (+ 3 5 7)) ; -> 390


;; 1.1.4 Compound Procedures
(defn square [x]
  (* x x))

(square 21) ; -> 441
(square (+ 2 5)) ; -> 49
(square (square 3)) ; -> 81

(defn sum-of-squares [x y]
  (+ (square x) (square y)))
(sum-of-squares 3 4) ; -> 35

(defn f [a]
 (sum-of-squares (+ a 1) (* a 2)))
(f 5) ; -> 136


;; 1.1.5 The Substitution Model for Procedure Activation


;; 1.1.6 Conditional Expression and Predicates
(defn abs [x]
  (cond
    (> x 0) x
    (= x 0) 0
    (< x 0) (- x)))

(defn abs [x]
  (cond
    (< x 0) (- x)
    true x))

(defn abs [x]
  (if (< x 0)
    (- x)
    x))

(defn >= [x y]
  (or (> x y) (= x y)))

(defn >= [x y]
  (not (< x y)))


;; Exercise 1.1
10 ; -> 10
(+ 5 3 4) ; -> 12
(- 9 1) ; -> 8
(/ 6 2) ; -> 3
(+ (* 2 4) (- 4 6)) ; -> 6
(def a 3)
(def b (+ a 1))
(+ a b (* a b)) ; -> 19
(= a b) ; -> false
(if (and (> b a) (< b (* a b)))
  b
  a) ; -> 4
(cond
  (= a 4) 6
  (= b 4) (+ 6 7 a)
  true 25) ; -> 16
(+ 2 (if (> b a) b a)) ; -> 6
(* (cond
     (> a b) a
     (< a b) b
     true -1)
   (+ a 1)) ; -> 16


;; Exercise 1.2
(/ (+ 5
      4
      (- 2
         (- 3
            (+ 6
               (/ 4 5)))))
   (* 3
      (- 6 2)
      (- 2 7))) ; -> -37/150


;; Exercise 1.3
(defn sum-largest-two-squares [a b c]
  (let [squares (map #(* % %) [a b c])]
       (- (apply + squares)
          (apply min squares))))
(sum-largest-two-squares 3 4 5) ; -> 41


;; Exercise 1.4
(defn a-plus-abs-b [a b]
  ((if (> b 0) + -) a b))
;; the conditional returns a function to apply to a,b


;; Exercise 1.5
(def p p)
(defn test [x y]
  (if (= x 0)
    0
    y))
(test 0 p) ; -> 0
;; returns 0 since predicate is evaluates first for builtin if
;; otherwise, we would get an unbound variable error


;; 1.1.7 Example: Square Roots by Newton's Method
(defn good-enough? [guess x]
  (< (Math/abs (- (square guess) x)) 0.001)) ; square is defined in 1.1.4
(defn average [x y]
  (/ (+ x y) 2))
(defn improve [guess x]
  (average guess (/ x guess)))
(defn sqrt-iter [guess x]
  (if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x)
               x)))
(defn sqrt [x]
  (sqrt-iter 1.0 x))
(sqrt 9) ; -> 3.00009155413138
(sqrt (+ 100 37)) ; -> 11.704699917758145
(sqrt (+ (sqrt 2) (sqrt 3))) ; -> 1.7739279023207892
(square (sqrt 1000)) ; -> 1000.000369924366


;; Exercise 1.6
(defn new-if [predicate then-clause else-clause]
  (cond
    predicate then-clause
    true else-clause))
(new-if (= 2 3) 0 5) ; -> 5
(new-if (= 1 1) 0 5) ; -> 0
(defn sqrt-iter [guess x]
  (new-if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x)
               x)))
(sqrt 9) ; -> stack overflow error
;; builtin if evaluates predicate before else clause;
;; new-if is a function, so all arguments get evaluated at function call
;; infinite recursion (or, more practically speaking, stack overflow)


;; Exercise 1.7
(defn sqrt-iter [guess x]
  (if (good-enough? guess x)
    guess
    (let [new-guess (improve guess x)]
      (if (< (/ (Math/abs (- guess new-guess)) guess) 0.001)
        guess
        (sqrt-iter new-guess
                   x)))))
(sqrt 0.0009) ; -> 0.04030062264654547
(sqrt 0.09) ; -> 0.3000299673226795
(sqrt 9) ; -> 3.00009155413138
(sqrt 900) ; -> 30.01177289789316
(sqrt 90000) ; -> 300.0007056063384


;; Exercise 1.8
(defn cube [x]
  (* x x x))
(defn good-enough? [guess x]
  (< (Math/abs (- (cube guess) x)) 0.001))
(defn improve-cubrt [guess x]
  (/ (+ (/ x (square guess)) (* 2 guess)) 3))
(defn cubrt-iter [guess x]
  (if (good-enough? guess x)
    guess
    (cubrt-iter (improve-cubrt guess x)
               x)))
(defn cubrt [x]
  (cubrt-iter 1.0 x))
(cubrt 27) ; -> 3.0000005410641766


;; 1.1.8 Procedures as Black-Box Abstractions
(defn square [x] (* x x))
(defn double [x] (+ x x))
(defn square [x]
  (Math/exp (double (Math/log x))))

(defn sqrt [x]
  (defn good-enough? [guess x]
    (< (abs (- (square guess) x)) 0.001))
  (defn improve [guess x]
    (average guess (/ x guess)))
  (defn sqrt-iter [guess x]
    (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))
  (sqrt-iter 1.0 x))

(defn sqrt [x]
  (defn good-enough? [guess]
    (< (abs (- (square guess) x)) 0.001))
  (defn improve [guess]
    (average guess (/ x guess)))
  (defn sqrt-iter [guess]
    (if (good-enough? guess)
      guess
      (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))

