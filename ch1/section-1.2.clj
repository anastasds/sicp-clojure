;; 1.2 Procedures and the Processes They Generate
;; 1.2.1 Linear Recusion and Iteration
(defn factorial [n]
  (if (= n 1)
    1
    (* n (factorial (- n 1)))))

(defn fact-iter [product counter max-count]
  (if (> counter max-count)
    product
    (fact-iter (* counter product)
               (+ counter 1)
               max-count)))
(defn factorial [n]
  (fact-iter 1 1 n))


;; Exercise 1.9
(defn + [a b]
  (if (= a 0)
    b
    (inc (+ (dec a) b))))
(defn + [a b]
  (if (= a 0)
    b
    (+ (dec a) (inc b))))
;; both recursive (by recursive call to overloaded +)


;; Exercise 1.10
(defn A [x y]
  (cond (= y 0) 0
        (= x 0) (* 2 y)
        (= y 1) 2
        true (A (- x 1)
                (A x (- y 1)))))
(A 1 10) ; -> 1024
(A 2 4) ; -> 65536
(A 3 3) ; -> 65536

(defn f [n] (A 0 n)) ; f(n) = 2n
(defn g [n] (A 1 n)) ; g(0) = 0, g(1) = 2, g(n) = 2^n
(defn h [n] (A 2 n)) ; h(0) = 0, h(1) = 2, h(n) = 2^2^2^.. (n times)
(defn k [n] (* 5 n n)) ; k(n) = 5n^2


;; 1.2.2 Tree Recursion
(defn fib [n]
  (cond (= n 0) 0
        (= n 1) 1
        true (+ (fib (- n 1))
                (fib (- n 2)))))

(defn fib-iter [a b count]
  (if (= count 0)
    b
    (fib-iter (+ a b) a (- count 1))))
(defn fib [n]
  (fib-iter 1 0 n))

(defn first-denomination [kinds-of-coins]
  (cond (= kinds-of-coins 1) 1
        (= kinds-of-coins 2) 5
        (= kinds-of-coins 3) 10
        (= kinds-of-coins 4) 25
        (= kinds-of-coins 5) 50))
(defn cc [amount kinds-of-coins]
  (cond (= amount 0) 1
        (or (< amount 0) (= kinds-of-coins 0)) 0
        true (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination kinds-of-coins))
                     kinds-of-coins))))
(defn count-change [amount]
  (cc amount 5))
(count-change 100) ; -> 292


;; Exercise 1.11
;; f(n) = n if n < 3
;; f(n) = f(n-1) + 2f(n-2) + 3f(n-3) if n >= 3
(defn ex1-11r [n]
  (if (< n 3)
    n
    (+ (ex1-11r (- n 1)) (* 2 (ex1-11r (- n 2))) (* 3 (ex1-11r (- n 3))))))

(defn ex1-11i [n]
  (defn ex1-11i-iter [a b c count]
    (if (< count 3)
      a
      (ex1-11i-iter (+ a (* 2 b) (* 3 c)) a b (- count 1))))
  (ex1-11i-iter 2 1 0 n))


;; Exercise 1.12
(defn pascal [row offset]
  (cond
    (or (< row 1) (< offset 1) (> offset row)) 0
    (= row offset) 1
    true (+ (pascal (- row 1) (- offset 1))
            (pascal (- row 1) offset))))


;; Exercise 1.13
;; sketch of proof:
;; - Let \phi = (1 + \sqrt{%5)/2, \psi = (1 - \sqrt{5})/2,
;; Fib(n) = (\phi^n - \psi^n) / \sqrt{5}.
;; - Verify base cases for n = 0, 1
;; - Prove by induction using straightforward algebraic rearrangement
;; after substituting definitions into Fib(n) + Fib(n-1)
;; to obtain Fib(n+1).
;; - Finally, use that Fib(n) - \phi^n/{5} = \psi^n/\sqrt{5}
;; and that \psi^n/\sqrt{5} < 1/2.


;; Exercise 1.14
;; space: O(n)
;; time: O(n^k), k = # types of coins ( = 5)


;; Exercise 1.15
;; Part A: by inspection, 5
;; Part B: logarithmic (base 3) since recursive call divides param by 3


;; 1.2.4 Exponentiation
(defn expt [b n]
  (if (= n 0)
    1
    (* b (expt b (- n 1)))))

(defn expt-iter [b counter product]
  (if (= counter 0)
    product
    (expt-iter b
               (- counter 1)
               (* b product)))) 
(defn expt [b n]
  (expt-iter b n 1))

(defn square [n]
  (* n n))
(defn even? [n]
  (= (rem n 2) 0))
(defn fast-expt [b n]
  (cond (= n 0) 1
        (even? n) (square (fast-expt b (/ n 2)))
        true (* b (fast-expt b (- n 1)))))


;; Exercise 1.16
(defn expt-iter [b n a]
   (cond (= n 0) a
         (even? n) (expt-iter (square b) (/ n 2) a)
         true (expt-iter  b (- n 1) (* a b))))
(defn fast-expt [b n]
  (expt-iter b n 1))


;; Exercise 1.17
(defn mult [a b]
  (cond (= b 0) 0
        (= b 1) a
        (even? b) (mult (* a 2) (/ b 2))
        true (+ a (mult a (- b 1)))))


;; Exercise 1.18
(defn mult-iter [m n a]
  (cond (= n 0) p
        (even? n) (mult (* m 2) (/ n 2))
        true (mult-iter m (- n 1) (+ n a))))
(defn fast-mult [m n]
  (mult-iter m n p))


