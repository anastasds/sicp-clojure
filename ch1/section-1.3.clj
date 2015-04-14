;; 1.3 Formulating Abstractions with Higher-Order Procedures
(defn cube [x]
  (* x x x))

;; 1.3.1 Procedures as Arguments
(defn sum-integers [a b]
  (if (> a b)
    0
    (+ a (sum-integers (+ a 1) b))))

(defn sum-cubes [a b]
  (if (> a b)
    0
    (+ (cube a) (sum-cubes (+ a 1) b))))

(defn pi-sum [a b]
  (if (> a b)
    0
    (+ (/ 1.0 (* a (+ a 2))) (pi-sum (+ a 4) b))))

(defn sum [term a next b]
  (if (> a b)
    0
    (+ (term a)
       (sum term (next a) next b))))

(defn inc [n] (+ n 1))
(defn sum-cubes [a b]
  (sum cube a inc b))
(sum-cubes 1 10) ; -> 3025

(defn identity [x] x)
(defn sum-integers [a b]
  (sum identity a inc b))
(sum-integers 1 10) ; -> 55

(defn pi-sum [a b]
  (defn pi-term [x]
    (/ 1.0 (* x (+ x 2))))
  (defn pi-next [x]
    (+ x 4))
  (sum pi-term a pi-next b))
(* 8 (pi-sum 1 1000)) ; -> 3.139592655589783

(defn integral [f a b dx]
  (defn add-dx [x] (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))
(integral cube 0 1 0.01) ; -> 0.24998750000000042
(integral cube 0 1 0.001) ; -> 0.249999875000001


;; Exercise 1.29
(defn simpsons-rule [f a b n]
  (def h (/ (- b a) n))
  (defn add-dx [x] (+ x h))
  (* (/ h 3)
     (+ (f a)
        (reduce + (map #(reduce * %) (map vector
                                          (take (- n 1) (cycle [4 2]))
                                          (map #(f (+ a (* % h))) (range 1 n)))))
        (f b))))

(defn simpsons-rule [f a b n]
  (def h (/ (- b a) n))
  (defn y [k] (f (+ a (* k h))))
  (defn simpsons-term [k]
    (* (cond (odd? k) 4
             (or (= k 0) (= k n)) 1
             (even? k) 2)
       (y k)))
  (/ (* h (sum simpsons-term 0 inc n)) 3))

(simpsons-rule cube 0 1 100) ; -> 1/4


;; Exercise 1.30
(defn sum [term a next b]
  (defn iter [a result]
    (if (> a b)
      result
      (iter (next a) (+ result (term a)))))
  (iter a 0))


;; Exercise 1.31
;; Part A
(defn product [term a next b]
  (if (> a b)
    1
    (* (term a)
       (product term (next a) next b))))
(defn factorial [n]
  (product identity 1 inc n))
(factorial 5) ; -> 120

(defn pi-product [a b]
  (defn pi-term [k]
    (cond  
      (= k 0) (/ 2 3)
      (odd? k) (/ (+ k 3) (+ k 2))
      (even? k) (/ (+ k 2) (+ k 3))))
  (defn pi-next [x] (inc x))
  (product pi-term a pi-next b))
(double (* 4 (pi-product 0 1000))) ; -> 3.140026946105016

;; Part B
(defn product [term a next b]
  (defn iter [a result]
    (if (> a b)
      result
      (iter (next a) (* result (term a)))))
  (iter a 1))
(double (* 4 (pi-product 0 1000))) ; -> 3.140026946105016


;; Exercise 1.32
;; Part A
(defn accumulate [combiner null-value term a next b]
  (if (> a b)
    null-value
    (combiner (term a)
              (accumulate combiner null-value term (next a) next b))))

(defn sum [term a next b]
  (accumulate + 0 term a next b))
(sum identity 0 inc 10) ; -> 55

(defn product [term a next b]
  (accumulate * 1 term a next b))
(product identity 1 inc 5) ; -> 120

;; Part B
(defn accumulate [combiner null-value term a next b]
  (defn iter [a result]
    (if (> a b)
      result
      (iter (next a) (combiner result (term a)))))
  (iter a null-value))

(sum identity 0 inc 10) ; -> 55
(product identity 1 inc 5) ; -> 120


;; Exercise 1.33
(defn filtered-accumulate [combiner null-value term a next b predicate?]
  (if (> a b)
    null-value
    (combiner
     (if (predicate? a) (term a) null-value)
     (filtered-accumulate combiner null-value term (next a) next b predicate?))))

;; Part A
(defn sum-squares-primes [a b]
  (filtered-accumulate + 0 square a inc b prime?))
(sum-squares-primes 3 8) ; -> 83

;; Part B
(defn sum-relatively-prime [n]
  (defn relatively-prime? [k]
    (= (gcd n k) 1))
  (filtered-accumulate + 0 identity 2 inc (dec n) relatively-prime?))
(sum-relatively-prime 10) ; -> 19


;; 1.3.2 Constructing Procedures Using Lambda
(fn [x] (+ x 4))

(fn [x] (/ 1.0 (* x (+ x  2))))

(defn pi-sum [a b]
  (sum (fn [x] (/ 1.0 (* x (+ x 2))))
       a
       (fn [x] (+ x 4))
       b))

(defn integral [f a b dx]
  (* (sum f
          (+ a (/ dx 2.0))
          (fn [x] (+ x dx))
          b)
     dx))

(defn plus4 [x] (+ x 4))

(def plus4 (fn [x] (+ x 4)))

((fn [x y z] (+ x y (square z))) 1 2 3) ; -> 12

(defn f [x y]
  (defn f-helper [a b]
    (+ (* x (square a))
       (* y b)
       (* a b)))
  (f-helper (+ 1 (* x y))
            (- 1 y)))

(defn f [x y]
  ((fn [a b]
     (+ (* x (square a))
        (* y b)
        (* a b)))
   (+ 1 (* x y))
   (- 1 y)))

(defn f [x y]
  (let [a (+ 1 (* x y))
        b (- 1 y)]
    (+ (* x (square a))
       (* y b)
       (* a b))))

(def x 5)
(+ (let [x 3]
     (+ x (* x 10)))
   x) ; -> 38

;; NOTE! Clojure's behavior here differs from how Scheme's is given in SICP.
;; The following will NOT output 12:
(def x 2)
(let [x 3
      y (+ x 2)]
  (* x y)) ; -> 15

;; However, the following /will/ output 12:
(def x 2)
(let [y (+ x 2)
      x 3]
  (* x y)) ; -> 12

(defn f [x y]
  (def a (+ 1 (* x y)))
  (def b (- 1 y))
  (+ (* x (square a))
     (* y b)
     (* a b)))


;; Exercise 1.34
(defn f [g]
  (g 2))
(f square) ; -> 4
(f (fn [z] (* z (+ z 1)))) ; -> 6
(f f) ; java.lang.ClassCastException: java.lang.Long cannot be cast to clojure.lang.IFn
;; that is, it tries to evaluate 2 as a function, which it isn't.


;; 1.3.3 Procedures as General Methods
