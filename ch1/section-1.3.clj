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
