;; 2.2 Hierarchical Data and the Closure Property
;; 2.2.1 Representing Sequences
(def one-through-four (list 1 2 3 4))
(first one-through-four) ; -> 1
(rest one-through-four) ; -> (2 3 4)
(first (rest one-through-four)) ; -> 2
(cons 10 one-through-four) ; (10 1 2 3 4)
(cons 5 one-through-four) ; (5 1 2 3 4)

(defn list-ref [items n]
  (if (= n 0)
    (first items)
    (list-ref (rest items) (- n 1))))
(def squares (list 1 4 9 16 25))
(list-ref squares 3) ; -> 16

(defn length [items]
  (if (empty? items)
    0
    (+ 1 (length (rest items)))))
(def odds (list 1 3 5 7))
(length odds) ; -> 4

(defn length [items]
  (defn length-iter [ a count]
    (if (empty? a)
      count
      (length-iter (rest a) (+ 1 count))))
  (length-iter items 0))

(concat squares odds) ; -> (1 4 9 16 25 1 3 5 7)
(concat odds squares) ; -> (1 3 5 7 1 4 9 16 25)

(defn append [list1 list2]
  (if (empty? list1)
    list2
    (cons (first list1) (append (rest list1) list2))))


;; Exercise 2.17
(defn last-pair [x]
  (cond (empty? x) x
        (empty? (rest x)) (first x)
        true (last-pair (rest x))))
(last-pair (list 23 72 149 34)) ; -> 34


;; Exercise 2.18
(defn reverse [x]
  (if (empty? x) x
      (concat (reverse (rest x)) [(first x)])))
(reverse (list 1 4 9 16 25)) ; -> (25 16 9 4 1)


;; Exercise 2.19
(defn cc [amount coin-values]
  (cond (= amount 0) 1
        (or (< amount 0) (no-more? coin-values)) 0
        true
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values))))

(defn no-more? [x] (empty? x))
(defn first-denomination [coin-values] (first coin-values))
(defn except-first-denomination [coin-values] (rest coin-values))

(def us-coins (list 50 25 10 5 1))
(def uk-coins (list 100 50 20 10 5 2 1 0.5))
(cc 100 us-coins) ; -> 292
(cc 100 uk-coins) ; -> 4563

;; permuting the list won't make a difference
(cc 100 (list 1 5 10 25 50)) ; -> 292


;; Exercise 2.20
(defn same-parity [x & rest]
  (cons x (filter (fn [z] (= (rem z 2) (rem x 2))) rest)))
(same-parity 1 2 3 4 5 6 7) ; -> (1 3 5 7)
(same-parity 2 3 4 5 6 7) ; -> (2 4 6)


(defn scale-list [items factor]
  (if (empty? items)
    nil
    (cons (* (first items) factor)
          (scale-list (rest items) factor))))
(scale-list (list 1 2 3 4 5) 10) ; -> 10 20 30 40 50)


(defn map [proc items]
  (if (empty? items)
    nil
    (cons (proc (first items))
          (map proc (rest items)))))
(defn abs [x] (Math/abs x))
(map abs (list -10 2.5 -11.6 17)) ; -> (10 2.5 11.6 17)
(map (fn [x] (* x x))
     (list 1 2 3 4)) ; -> (1 4 9 16)

(defn scale-list [items factor]
  (map (fn [x] (* x factor))
       items))


;; Exercise 2.21
(defn square [x] (* x x))
(defn square-list [items]
  (if (empty? items)
    nil
    (cons (square (first items)) (square-list (rest items)))))
(defn square-list [items]
  (map (fn [x] (* x x)) items))


;; Exercise 2.22
(defn square-list [items]
  (defn iter [things answer]
    (if (empty? things)
      answer
      (iter (rest things)
            (cons (square (first things))
                  answer))))
  (iter items nil))

;; it's reversed because it takes it iterates builds the new list
;; by successively appending the first element of (rest items)
;; i.e. it does what it does because it does what executes as written.


(defn square-list [items]
  (defn iter [things answer]
    (if (empty? things)
      answer
      (iter (rest things)
            (cons answer
                  (square (first things))))))
  (iter items nil))

;; the first argument to cons is a list. the following works:
(defn square-list [items]
  (defn iter [things answer]
    (if (empty? things)
      answer
      (iter (rest things)
            (concat answer
                  (list (square (first things)))))))
  (iter items nil))
(square-list (list 1 2 3 4 5))
