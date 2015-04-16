;; 2.1 Introduction to Data Abstraction
;; 2.1.1 Example: Arithmetic Operations for Rational Numbers
(defn add-rat [x y]
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(defn sub-rat [x y]
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(defn mul-rat [x y]
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))
(defn div-rat [x y]
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))
(defn equal-rat? [x y]
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

;; note: Clojure does have cons, but it takes an element and a list
;; e.g. (cons 1 [2]) -> (1 2)
;; there is no concept of pairs; instead, here we use list for cons,
;; first for car, second for cdr
;; for arbitrary lists later, we'll use rest instead of second
(def x (list 1 2))
(first x) ; -> 1
(second x) ; -> 2

(def x (list 1 2))
(def y (list 3 4))
(def z (list x y))
(first (first z)) ; -> 1
(first (second z)) ; -> 3

(defn make-rat [n d] (list n d))
(defn numer [x] (first x))
(defn denom [x] (second x))

(defn print-rat [x]
  (println (numer x) "/" (denom x)))

(def one-half (make-rat 1 2))
(print-rat one-half) ; -> 1 / 2

(def one-third (make-rat 1 3))
(print-rat one-third) ; -> 1 / 3

(print-rat (add-rat one-half one-third)) ; -> 5 / 6
(print-rat (mul-rat one-half one-third)) ; -> 1 / 6
(print-rat (add-rat one-third one-third)) ; -> 6 / 9

(defn gcd [a b]
  (if (= b 0)
    a
    (gcd b (rem a b))))
(defn make-rat [n d]
  (let [g (gcd n d)]
    (list (/ n g) (/ d g))))

(print-rat (add-rat one-third one-third)) ; -> 2 / 3


;; Exercise 2.1
(defn make-rat [n d]
  (let [g (gcd (Math/abs n) (Math/abs d))]
    (if (< (* n d) 0)
      (list (/ (- (Math/abs n)) g) (/ (Math/abs d) g))
      (list (/ (Math/abs n) g) (/ (Math/abs d) g)))))
(print-rat (make-rat 1 2))


;; 2.1.2 Abstraction Barriers
(defn make-rat [n d]
  (list n d))
(defn numer [x]
  (let [g (gcd (first x) (second x))]
    (/ (first x) g)))
(defn denom [x]
  (let [g (gcd (first x) (second x))]
    (/ (second x) g)))


;; Exercise 2.2
(defn make-point [x y] (list x y))
(defn x-point [point] (first point))
(defn y-point [point] (second point))

(defn make-segment [start-segment end-segment] (list start-segment end-segment))
(defn start-segment [segment] (first segment))
(defn end-segment [segment] (second segment))

(defn average [u v] (/ (+ u v) 2))
(defn midpoint-segment [segment]
  (make-point (average (x-point (start-segment segment)) (x-point (end-segment segment)))
              (average (y-point (start-segment segment)) (y-point (end-segment segment)))))

(defn print-point [p]
  (println "(" (x-point p) "," (y-point p) ")"))

(print-point (midpoint-segment (make-segment (make-point 0 0) (make-point 2 1)))) ; -> ( 1 , 1/2 )


;; Exercise 2.3
;; top-left, bottom-right corners as points
(defn make-rect [p1 p2]
  (list p1 p2))
(defn perimeter-rect [rect]
  (* 2 (+ (- (x-point (second rect)) (x-point (first rect)))
          (- (y-point (first rect)) (y-point (second rect))))))
(defn area-rect [rect]
  (* (- (x-point (second rect)) (x-point (first rect)))
     (- (y-point (first rect)) (y-point (second rect)))))

;; same principle, but using x,y coordinates instead of points
(defn make-rect [x1 y1 x2 y2]
  (list (make-point x1 y1) (make-point x2 y2)))


;; 2.1.3 What is Meant by Data?
(defn cons [x y]
  (defn dispatch [m]
    (cond (= m 0) x
          (= m 1) y
          true (throw (Throwable. (str "Argument not 0 or 1 -- CONS " m)))))
  dispatch)
(defn car [z] (z 0))
(defn cdr [z] (z 1))


;; Exercise 2.4
(defn cons [x y]
  (fn [m] (m x y)))
(defn car [z]
  (z (fn [p q] p)))
(car (cons 4 5)) ; -> 4

(defn cdr [z]
  (z (fn [p q] q)))
(cdr (cons 4 5)) ; -> 5


;; Exercise 2.5
(defn log-base [b x]
  (Math/round (/ (Math/log x) (Math/log b))))
(defn cons [a b]
  (*' (bigint (Math/pow 2 a)) (bigint (Math/pow 3 b))))
(defn car [z]
  (if (= (rem z 3) 0)
    (car (/ z 3))
    (log-base 2 z)))
(defn cdr [z]
  (if (= (rem z 2) 0)
    (cdr (/ z 2))
    (log-base 3 z)))
(cdr (cons 4 5)) ; -> 5
(car (cons 4 5)) ; -> 4


;; Exercise 2.6
(def zero (fn [f] (fn [x] x))) ; 0 = λf.λx.x
(defn add-1 [n] (fn [f] (fn [x] (f ((n f) x))))) ; add-1 = λn.λf.λx.f(nf(x)) = λn.λf.λx.(n+1)f(x)
(def one (fn [f] (fn [x] (f x))))  ; 1 = λf.λx.f(x)
(def two (fn [f] (fn [x] (f (f x)))))  ; 2 = λf.λx.f(f(x))
(defn add [m n] (fn [f] (fn [x] ((m f) ((n f) x))))) ; + = λm.λn.λf.λx.((m+n)f)(x)


;; 2.1.4 Extended Exercise: Interval Arithmetic
(defn add-interval [x y]
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(defn mul-interval [x y]
  (let [p1 (* (lower-bound x) (lower-bound y))
        p2 (* (lower-bound x) (upper-bound y))
        p3 (* (upper-bound x) (lower-bound y))
        p4 (* (upper-bound x) (upper-bound y))]
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(defn div-interval [x y]
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

;; Exercise 2.7
;; Note: as remarked before, cons/car/cdr will be replaced with list/first/second for now
(defn make-interval [a b] (list a b))

(defn lower-bound [interval] (first interval))
(defn upper-bound [interval] (second interval))


;; Exercise 2.8
(defn sub-interval [x y]
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))


;; Exercise 2.9
(defn width [x] (/ (- (upper-bound x) (lower-bound x)) 2))
;; let m(x) = (lower-bound x), M(x) = (upper-bound x), w(x) = (M(x) - m(x)) / 2
;; (addition) by definition, m(x+y) = m(x) + m(y), M(x+y) = M(x) + M(y)
;; w(x+y) = [M(x+y) - m(x+y)]/2
;;        = [M(x) - m(x)]/2 + [M(y) - m(y)]/2
;;        =  w(x) + w(y)
;; (subtraction) by definition, m(x-y) = m(x) - M(y), M(x-y) = M(x) - m(y)
;; w(x-y) = |[M(x-y) - m(x-y)]/2|
;;        = |[m(x) - M(y) - M(x) + m(y)]/2|
;;        = |[m(x) - M(x)]/2 - (M(y) - m(y)]/2|
;;        = |-w(x) - w(y)|
;;        = |w(x) + w(y)|
;;        = w(x) + w(y)
;; (multiplication) [1,2]*[1,3] = [1,6]; [1,2]*[2,4] = [2,8]; * not well-defined for w
;; (division) defined in terms of multiplication, so again / also not well-defined for w


;; Exercise 2.10
(defn div-interval [x y]
  (if (< (* (lower-bound y) (upper-bound y)) 0)
    (throw (Throwable. (str "Division by zero!")))
    (mul-interval x
                  (make-interval (/ 1.0 (upper-bound y))
                                 (/ 1.0 (lower-bound y))))))


;; Exercise 2.11
(defn mul-interval [x y]
   (let [x-min (lower-bound x)
         x-max (upper-bound x)
         y-min (lower-bound y)
         y-max (upper-bound y)]
     (cond (and (>= x-min 0)
                (>= x-max 0)
                (>= y-min 0)
                (>= y-max 0)) ; [+, +] * [+, +]
           (make-interval (* x-min y-min) (* x-max y-max)) 
           (and (>= x-min 0)
                (>= x-max 0)
                (<= y-min 0)
                (>= y-max 0)) ; [+, +] * [-, +]
           (make-interval (* x-max y-min) (* x-max y-max))
           (and (>= x-min 0)
                (>= x-max 0)
                (<= y-min 0)
                (<= y-max 0)) ; [+, +] * [-, -]
           (make-interval (* x-max y-min) (* x-min y-max))
           (and (<= x-min 0)
                (>= x-max 0)
                (>= y-min 0)
                (>= y-max 0)) ; [-, +] * [+, +]
           (make-interval (* x-min y-max) (* x-max y-max))
           (and (<= x-min 0)
                (>= x-max 0)
                (<= y-min 0)
                (>= y-max 0)) ; [-, +] * [-, +]
           (make-interval (min (* x-max y-min) (* x-min y-max))
                          (max (* x-min y-min) (* x-max y-max)))
           (and (<= x-min 0)
                (>= x-max 0)
                (<= y-min 0)
                (<= y-max 0)) ; [-, +] * [-, -]
           (make-interval (* x-max y-min) (* x-min y-min))
           (and (<= x-min 0)
                (<= x-max 0)
                (>= y-min 0)
                (>= y-max 0)) ; [-, -] * [+, +]
           (make-interval (* x-min y-max) (* x-max y-min))
           (and (<= x-min 0)
                (<= x-max 0)
                (<= y-min 0)
                (>= y-max 0)) ; [-, -] * [-, +]
           (make-interval (* x-min y-max) (* x-min y-min))
           (and (<= x-min 0)
                (<= x-max 0)
                (<= y-min 0)
                (<= y-max 0)) ; [-, -] * [-, -]
           (make-interval (* x-max y-max) (* x-min y-min)))))


(defn make-center-width [c w]
  (make-interval (- c w) (+ c w)))
(defn center [i]
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(defn width [i]
  (/ (- (upper-bound i) (lower-bound i)) 2))


;; Exercise 2.12
(defn make-center-percent [m p]
  (make-interval (- m (* m p)) (+ m (* m p))))
(defn percent [i]
  (/ (width i) (center i)))


;; Exercise 2.13
;; given intervals m1 +- t1, m2 +- t2, after tedious algebra and dropping negligible terms,
;; product: m1m2 +- |t1+t2|


(defn par1 [r1 r2]
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))
(defn par2 [r1 r2]
  (let [one (make-interval 1 1)]
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))


;; Exercise 2.14
(par1 (make-interval 1 3) (make-interval 2 4)) ; -> (0.2857142857142857 4.0)
(par2 (make-interval 1 3) (make-interval 2 4)) ; -> (0.6666666666666666 1.7142857142857144)
;; due to compounding calculated error when a value is used more than once in formula


;; Exercise 2.15
;; as above, due to compounding error


;; Exercise 2.16
;; nope; not every formula can be written using each of its distinct terms exactly once
;; proof not given.
