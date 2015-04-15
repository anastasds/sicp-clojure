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
(defn average [x y]
  (/ (+ x y) 2))
(defn positive? [x] (> x 0))
(defn negative? [x] (< x 0))
(defn close-enough? [x y]
  (< (Math/abs (- x y)) 0.001))
(defn search [f neg-point pos-point]
  (let [midpoint (average neg-point pos-point)]
    (if (close-enough? neg-point pos-point)
      midpoint
      (let [test-value (f midpoint)]
        (cond (positive? test-value) (search f neg-point midpoint)
              (negative? test-value) (search f midpoint pos-point)
              true midpoint)))))
(defn half-interval-method [f a b]
  (let [a-value (f a)
        b-value (f b)]
    (cond (and (negative? a-value) (positive? b-value)) (search f a b)
          (and (negative? b-value) (positive? a-value)) (search f b a)
          true (throw (Throwable. (str "Values are not of opposite sign " a " " b))))))
(defn sin [x] (Math/sin x))
(half-interval-method sin 2.0 4.0) ; -> 3.14111328125
(half-interval-method (fn [x] (- (* x x x) (* 2 x) 3))
                      1.0
                      2.0) ; -> 1.89306640625

(def tolerance 0.00001)
(defn cos [x] (Math/cos x))
(defn abs [x] (Math/abs x))
(defn fixed-point [f first-guess]
  (defn close-enough? [v1 v2]
    (< (abs (- v1 v2)) tolerance))
  (defn try-guess [guess]
    (let [next (f guess)]
      (if (close-enough? guess next)
        next
        (try-guess next))))
  (try-guess first-guess))
(fixed-point cos 1.0) ; -> 0.7390822985224024 - NOTE! last decimal point differs by 1 from SICP example
(fixed-point (fn [y] (+ (sin y) (cos y))) 1.0) ; -> 1.2587315962971173

(defn sqrt [x]
  (fixed-point (fn [y] (/ x y)) 1.0)) ; note: infinite loop
(defn sqrt [x]
  (fixed-point (fn [y] (average y (/ x y))) 1.0))


;; Exercise 1.35
;; x = 1 + 1/x
;; x^2 = x + 1
;; x^2 - x - 1 = 0
;; x = \phi
(fixed-point (fn [x] (+ 1 (/ 1 x))) 1.0) ; -> 1.618032786885245


;; Exercise 1.36
(defn fixed-point [f first-guess]
  (defn close-enough? [v1 v2]
    (< (abs (- v1 v2)) tolerance))
  (defn try-guess [guess]
    (println guess)
    (let [next (f guess)]
      (if (close-enough? guess next)
        next
        (try-guess next))))
  (try-guess first-guess))
(defn log [x] (Math/log x))
(fixed-point (fn [x] (/ (log 1000) (log x))) 2.0) ; -> 4.555532270803653
;; 34 iterations without average damping
(fixed-point (fn [x] (average x (/ (log 1000) (log x)))) 2.0) ; -> 4.555537551999825
;; 9 iterations with average damping


;; Exercise 1.37
;; Part A
(defn cont-frac [n d k]
  (defn cont-frac-iter [i result]
    (if (= i 0)
      result
      (cont-frac-iter (dec i) (/ (n i) (+ (d i) result)))))
  (cont-frac-iter k 0))
(/ 1 (cont-frac (fn [x] 1.0)
                (fn [x] 1.0)
                12)) ; -> 1.6180555555555558

;; Part B
(defn cont-frac [n d k]
  (defn cont-frac-recur [i]
    (if (< i k)
      (/ (n i) (+ (d i) (cont-frac-recur (inc i))))
      (/ (n i) (d i))))
  (cont-frac-recur 1))
(/ 1 (cont-frac (fn [x] 1.0)
                (fn [x] 1.0)
                12)) ; -> 1.6180555555555558


;; Exercise 1.38
(+ 2 (cont-frac (fn [k] 1.0)
                (fn [k] (if (= (rem (+ k 1) 3) 0)
                          (/ (* 2 (+ k 1)) 3)
                          1))
                10)) ; -> 2.7182817182817183


;; Exercise 1.39
(defn tan-cf [x k]
  (cont-frac (fn [i] (if (= i 1) x (- (* x x))))
             (fn [i] (- (* 2 i) 1))
             k))

;; 1.3.4 Procedures as Returned Values
(defn square [x] (* x x))
(defn average-damp [f]
  (fn [x] (average x (f x))))
((average-damp square) 10) ; -> 55

(defn sqrt [x]
  (fixed-point (average-damp (fn [y] (/ x y))) 1.0))

(defn cube-root [x]
  (fixed-point (average-damp (fn [y] (/ x (square y)))) 1.0))

(def dx 0.00001)
(defn deriv [g]
  (fn [x] (/ (- (g (+ x dx)) (g x))
             dx)))
(defn cube [x] (* x x x))
((deriv cube) 5) ; -> 75.00014999664018

(defn newton-transform [g]
  (fn [x]
    (- x (/ (g x) ((deriv g) x)))))
(defn newtons-method [g guess]
  (fixed-point (newton-transform g) guess))
(defn sqrt [x]
  (newtons-method (fn [y] (- (square y) x)) 1.0))

(defn fixed-point-of-transform [g transform guess]
  (fixed-point (transform g) guess))
(defn sqrt [x]
  (fixed-point-of-transform (fn [y] (/ x y)) average-damp 1.0))
(defn sqrt [x]
  (fixed-point-of-transform (fn [y] (- (square y) x))
                            newton-transform
                            1.0))


;; Exercise 1.40
(defn cubic [a b c x]
  (+ (cube x)
     (* a (square x))
     (* b x)
     c))
(newtons-method #(cubic 3 3 1 %) 1) ; -> -0.9999755158323895
(newtons-method (partial cubic 3 3 1) 1) ; equivalent


;; Exercise 1.41
(defn double [g]
  (defn double-evaluate [g x]
    (g (g x)))
  (partial double-evaluate g))
(((double (double double)) inc) 5) ; -> 21


;; Exercise 1.42
(defn compose [f g]
  (defn compose-evaluate [f g x]
    (f (g x)))
  (partial compose-evaluate f g))
((compose square inc) 6) ; -> 49


;; Exercise 1.43
(defn repeated [f n]
  (defn repeated-iter [k result]
    (if (> k n)
      result
      (compose f result)))
  (repeated-iter 1 f))
((repeated square 2) 5) ; -> 625


;; Exercise 1.44
(defn smooth [f]
  (def dx 0.001)
  (defn smooth-evaluate [f x]
    (/ (+ (f (- x dx))
          (f x)
          (f (+ x dx)))
       3))
  (partial smooth-evaluate f))
;; for example, (((repeated smooth 2) cube) 2)


;; Exercise 1.45
(defn nth-root [x n]
  (fixed-point-of-transform
   (fn [y] (/ x (Math/pow y (dec n))))
   ((partial repeated average-damp (dec n)))
   1.0))


;; Exercise 1.46
(defn iterative-improve [good-enough? improve]
  (defn iter [guess]
    (if (good-enough? guess)
      guess
      (iter (improve guess))))
  (partial iter))
(defn sqrt [x]
  ((iterative-improve
    (fn [y] (< (abs (- (square y) x)) 0.001))
    (fn [y] (average y (/ x y))))
   1.0))
(defn fixed-point [f first-guess]
  ((iterative-improve
    (fn [y] (< (abs (- y (f y))) 0.001))
    (fn [y] (f y)))
   first-guess))
