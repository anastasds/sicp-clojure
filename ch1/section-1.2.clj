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


;; Exercise 1.19
(defn fib-iter [a b p q count]
  (cond (= count 0) b
        (even? count)(fib-iter a
                               b
                               (+ (square p) (square q))
                               (+ (* 2 p q) (square q))
                               (/ count 2))
        true (fib-iter (+ (* b q) (* a q) (* a p))
                       (+ (* b p) (* a q))
                       p
                       q
                       (- count 1))))
(defn fib [n]
  (fib-iter 1 0 0 1 n))


;; 1.2.3 Greatest Common Divisors
(defn gcd [a b]
  (if (= b 0)
    a
    (gcd b (rem a b))))


;; Esxercise 1.20
;; normal order - 18 rems;
(if (= 40 0)
  206
  (if (= (rem 206 40) 0)
    40
    (if (= (rem 40 (rem 206 40)) 0)
      (rem 206 40)
      (if (= (rem (rem 206 40) (rem (rem 206 40) (rem 40 (rem 206 40)))) 0)
        (rem 40 (rem 206 40))
        (if (= (rem (rem 40 (rem 206 40)) (rem (rem 206 40) (rem (rem 206 40) (rem 40 (rem 206 40))))) 0) 0
            _ ; value here is passed in as argument; no more rems
            (gcd _ _))))))

;; applicative order - 4 rems:
(if (= 40 0)
  206
  (if (= (rem 206 40) 0)
    40
    (if (= (rem 40 6) 0)
      6
      (if (= (rem 6 4) 0)
        4
        (if (= (rem 4 2) 0)
          2
          (gcd 0 0)))))) ; -> 0


;; 1.2.6 Example: Testing for Primality
(defn divides? [a b]
  (= (rem b a) 0))
(defn find-divisor [n test-divisor]
  (cond (> (square test-divisor) n) n
        (divides? test-divisor n) test-divisor
        true (find-divisor n (+ test-divisor 1))))
(defn smallest-divisor [n]
  (find-divisor n 2))
(defn prime? [n]
  (= n (smallest-divisor n)))

(defn expmod [base exp m]
  (cond (= exp 0) 1
        (even? exp) (rem (square (expmod base (/ exp 2) m))
                    m)
        true (rem (* base (expmod base (- exp 1) m))
                  m)))
(defn fermat-test [n]
  (defn try-it [a]
    (= (expmod a n n) a))
  (try-it (+ 1 (rand-int (- n 1)))))
(defn fast-prime? [n times]
  (cond (= times 0) true
        (fermat-test n) (fast-prime? n (- times 1))
        true false))


;; Exercise 1.21
(smallest-divisor 199) ; -> 199
(smallest-divisor 1999) ; -> 1999
(smallest-divisor 19999) ; -> 7


;; Exercise 1.22
(defn timed-prime-test [n]
  (println n)
  (println (time (prime? n))))

(defn search-for-primes [m n]
  (if (and (odd? m) (prime? m))
    (timed-prime-test m))
  (if (< m n)
    (search-for-primes (+ 1 m) n)))

(search-for-primes 1000 1025) ; => 1009 (0.023651ms), 1013 (0.020081ms), 1019 (0.019635ms)
(search-for-primes 10000 10050) ; => 10007 (0.063367ms), 10009 (0.058458ms), 10037 (0.058458ms)
(search-for-primes 100000 100075) ; 100003 (0.214198ms), 100019 (0.180283ms), 100043 (0.216875ms)
(search-for-primes 1000000 1000100) ; 100003 (0.61805ms), 1000033 (0.649809ms), 1000037 (0.656874ms)
;; (note: flipped (x,y) values and did quadratic fit using online tool,
;; because while emacs calc does do curve fitting, it does not give the r^2 value for quadratic fits)
;; 
;; Mode: normal x,y analysis
;; Polynomial degree 2, 12 x,y data pairs.
;; Correlation coefficient (r^2) = 0.9957995249675982
;; Standard error = 29732.40855574766
;; Coefficient output form: simple list:
;;
;;   -2.5852655546510796e+003
;;    5.1659092409253586e+004
;;    2.3449696194720506e+006
;;
;; Copyright (c) 2013, P. Lutus -- http://arachnoid.com. All Rights Reserved.


;; Exercise 1.23
(defn next [n]
  (if (= n 2)
    3
    (+ n 2)))
(defn find-divisor [n test-divisor]
  (cond (> (square test-divisor) n) n
        (divides? test-divisor n) test-divisor
        true (find-divisor n (next test-divisor))))
(defn smallest-divisor [n]
  (find-divisor n 2))

(search-for-primes 1000 1020) ; => 1009 (0.029899ms), 1013 (0.027667ms), 1019 (0.027668ms)
(search-for-primes 10000 10038) ; => 10007 (0.085679ms), 10009 (0.081663ms), 10037 (0.085233ms)
(search-for-primes 100000 100044) ; 100003 (0.245881ms), 100019 (0.272656ms), 100043 (0.222231ms)
(search-for-primes 1000000 1000038) ; 100003 (0.38511ms), 1000033 (0.378417ms), 1000037 (0.38511ms)
;; Mode: normal x,y analysis
;; Polynomial degree 2, 12 x,y data pairs.
;; Correlation coefficient (r^2) = 0.9720824252072567
;; Standard error = 76651.33659653936
;; Coefficient output form: mathematical function:
;;
;; f(x) =  1.5422537511362165e+005 * x^0
;;      + -4.0109068981622611e+006 * x^1
;;      +  1.6080571631175241e+007 * x^2
;;
;; Copyright (c) 2013, P. Lutus -- http://arachnoid.com. All Rights Reserved.
;;
;; the coefficient on the quadratic term is ~69% of the previous value
;; it is greater than half due to the overhead of the function call.
;; starting by checking at 3 and doing (+ test-divisor 2) instead of (next test-divisor)
;; significantly reduces runtime further, as an illustration


;; Exercise 1.24
(defn timed-prime-test [n]
  (println n)
  (println (time (fast-prime? n (int (Math/log n))))))
(timed-prime-test 1009) ; 0.094604ms
(timed-prime-test 1013) ; 0.103975ms
(timed-prime-test 1019) ; 0.100852ms
(timed-prime-test 10007) ; 0.161541ms
(timed-prime-test 10009) ; 0.156186ms
(timed-prime-test 10037) ; 0.182961ms
(timed-prime-test 100003) ; 0.217767ms
(timed-prime-test 100019) ; 0.220892ms
(timed-prime-test 100043) ; 0.28292ms
(timed-prime-test 1000003) ; 0.326652ms
(timed-prime-test 1000033) ; 0.320404ms
(timed-prime-test 1000037) ; 0.307017ms
;; log fit expected; found with r^2 > 0.95


;; Exercise 1.25
(defn expmod [base exp m]
  (rem (fast-expt base exp) m))
;; slower because calculates the base^expt in full before taking modulo
;; while original took module before;
;; modular arithmetic is well-defined and doing it this way is much slower
;; since much bigger numbers are involved


;; Exercise 1.26
(defn expmod [base exp m]
  (cond (= exp 0) 1
        (even? exp) (rem (* (expmod base (/ exp 2) m)
                                  (expmod base (/ exp 2) m))
                               m)
        true (rem (* base (expmod base (- exp 1) m))
                       m)))
;; in the even? case of the cond, expmod is recursively called twice
;; log_2(2^n) = n


;; Exercise 1.27
(defn rem' [a b]
  (rem (bigint a) (bigint b)))
(defn square' [n]
  (*' n n))
(defn expmod' [base exp m]
  (cond (= exp 0) 1
        (even? exp) (rem' (square' (expmod' base (/ exp 2) m))
                    m)
        true (rem' (*' base (expmod' base (- exp 1) m))
                  m)))
(defn fermat-carmichael-test [n]
  (loop [k 2 t 0 f 0]
    (if (= k n)
      (println t "passed" f "failed")
      (if (= (expmod' k n n) k)
        (recur (inc k) (inc t) f)
        (recur (inc k) t (inc f))))))

(fermat-carmichael-test 561) ; 559 passed 0 failed
(fermat-carmichael-test 1105) ; 1103 passed 0 failed
(fermat-carmichael-test 1729) ; 1727 passed 0 failed
(fermat-carmichael-test 2465) ; 2463 passed 0 failed
(fermat-carmichael-test 2821) ; 2819 passed 0 failed
(fermat-carmichael-test 6601) ; 6599 passed 0 failed


;; Exercise 1.28
(defn miller-rabin-check [n m]
  (if (and (not (= n 1))
           (not (= m (- n 1)))
           (= (rem' n m) 1))
    (rem' (square' n) m)))
(defn miller-rabin-expmod [base exp m]
  (cond (= exp 0) 1
        (even? exp) (miller-rabin-check (expmod' base (/ exp 2) m))
        true (rem' (*' base (expmod' base (- exp 1) m))
                  m)))
(defn miller-rabin-test [n]
  (loop [k 2 t 0 f 0]
    (if (= k n)
      (println t "passed" f "failed")
      (if (= (expmod' k (- n 1) n) 1)
        (recur (inc k) (inc t) f)
        (recur (inc k) t (inc f))))))

(miller-rabin-test 561) ; -> 219 passed 240 failed
(miller-rabin-test 1105) ; -> 767 passed 336 failed
(miller-rabin-test 1729) ; -> 1295 passed 432 failed
(miller-rabin-test 2465) ; -> 1791 passed 672 failed
(miller-rabin-test 2821) ; -> 2158 passed 660 failed
(miller-rabin-test 6601) ; -> 5279 passed 1320 failed
