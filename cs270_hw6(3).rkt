#lang racket
;Put Your Name Here: Tram Phan

#|
CS 270 Math Foundations of CS
Homework 6
Created by Professor Bruce Char, Professor Mark Boady, Professor Jeremy Johnson, and Steve Earth

Submit in BBLearn.

Once you write a function, you may use it in later questions.

Important Rules:
1.) You may not use loop constructs like while/for/sum. If used, your answer will get a zero.
2.) If the instructions state something must be recursive, you will recieve a zero if it is not recursive.
    Recursive helper functions are allowed (the main function not being recursive).
3.) You may not use the set! command. If used, your answer will get a zero.
4.) Using If/Cond to explicitly pass tests instead of following the instructions
    will always result in a zero for that question.

  Peano arithmetic

    In words:  A peano number (aka "pnum" for short) is either zero, or, recursively, the symbol s cons-ed 
    to a peano number.

    Formally:  Pnum := null | (cons s [pnum])
|#

(require racket/contract)
(require rackunit)
(require rackunit/text-ui)

#|

Peano arithmetic.  Note that for these problems, uppercase designates a pnum and lowercase is the integer it represents.
For instance, we might have M = '(s s s) in which case m = 3.

|#

;The zero element is the null list
;returns true if the list represents the number zero
;input-contract: (list? N)
;output-contract: (boolean? (zero? N)). that is, zero? is the predicate for whether the pnum N represents 0
(define zero? null?)

;Checks if the input is a list representing a peano number
;input-contract: (list? N)
;output-contract: (boolean? (nat? N)). i.e. nat? is the predicate checking whether N is a pnum.
(define (nat? N)
  (cond
    [(zero? N) #t]
    [(cons? N) (and (equal? (first N) 's) (nat? (rest N)))]
    [else #f]))

;Increment a peano number by adding 1
;input-contract: (nat? N)
;output-contract: (nat? (succ N)). i.e. (succ N) is the pnum representing n+1
(define (succ N) (cons 's N))

;Decrement a peano number by subtracting 1
;input-contract: (nat? N)
;output-contract: (nat? (pred N)).  i.e. (pred N) is the pnum representing n-1 provided n>0
(define (pred N) (if (zero? N) null (rest N)))

;Define a collection of common numbers
(define zero null)
(define one (succ zero))
(define two (succ one))
(define three (succ two))
(define four (succ three))
(define five (succ four))
(define six (succ five))
(define seven (succ six))
(define eight (succ seven))
(define nine (succ eight))
(define ten (succ nine))

; Addition of Peano numbers
; input-contract: (and (nat? M) (nat? N)). i.e. M and N are pnums representing the integers m and n.
; output-contract: (nat? (plus M N)).   i.e. (plus M N) is the pnum representing m+n.
(define (plus M N) (if (zero? M) N (succ (plus (pred M) N))))

; Multiplication of Peano numbers
; input-contract: (and (nat? M) (nat? N)). M,N are the pnum representations of the integers m,n.
; output-contract: (nat? (mult M N)). i.e. (mult M N) is the pnum representing m*n.
(define (mult M N) (if (zero? M) M (plus N (mult (pred M) N))))

; Comparison of Peano numbers
; input-contract: (and (nat? M) (nat? N)). M,N are the pnum representations of the integers m,n.
; output-contract: (boolean? (ltnat? M N)). that is, (ltnat? M N) is the predicate for m<n.
(define (ltnat? M N)
  (cond
    [(zero? N) #f]
    [(zero? M) #t]
    [else (ltnat? (pred M) (pred N))]))

;--------------- Question 1.  Implement subtraction of Peano numbers. --------------
;See specification below.

; Subtraction of Peano numbers
; input-contract: (and (nat? M) (nat? N))
; output-contract: (nat? (sub M N)). (sub M N) is the pnum of m-n if m >= n; otherwise return zero
; Requirements: You may not convert to or from traditional integers, nor use length. Your function must be recursive.
(define (sub M N)
  (cond
    ((zero? N) M)
    ((zero? M) zero)
    (else (sub (pred M) (pred N)))))

;Test Bed
(display "Question 1 - Subtraction (10 points)\n")
(define-test-suite peano-subtract
  (check-equal? (sub ten ten) zero)
  (check-equal? (sub ten two) eight)
  (check-equal? (sub nine nine) zero)
  (check-equal? (sub nine one) eight)
  (check-equal? (sub eight six) two)
  (check-equal? (sub eight five) three)
  (check-equal? (sub seven one) six)
  (check-equal? (sub seven five) two)
  (check-equal? (sub six six) zero)
  (check-equal? (sub six two) four)
)
(define q1_score (- 10 (run-tests peano-subtract 'verbose)))

;--------------- Question 2.  Implement Quotient of Peano numbers. --------------

; Division of Peano numbers
; input-contract: (and (nat? M) (nat? N) (not (zero? N)))
; output-contract: (nat? (ltnat? M N))
; Returns a Peano number whose value q is the quotient of m divided by n.
; Remember if x/y then quotient q and remainder r meet the following requirement:
; x = q*y + r with 0 <= r < x. Return floor(m/n) otherwise
; Requirements: You may not convert to or from traditional integers, nor use length. Your function must be recursive.
(define (div M N)
  (if (ltnat? M N) zero
  (succ (div (sub M N) N))))

;Test Bed
(display "Question 2 - Division (10 points)\n")
(define-test-suite peano-div
  (check-equal? (div ten ten) one)
  (check-equal? (div ten two) five)
  (check-equal? (div nine three) three)
  (check-equal? (div nine one) nine)
  (check-equal? (div eight six) one)
  (check-equal? (div eight four) two)
  (check-equal? (div one seven) zero)
  (check-equal? (div seven five) one)
  (check-equal? (div six six) one)
  (check-equal? (div six two) three)
)
(define q2_score  (- 10 (run-tests peano-div 'verbose)))


;--------------- Question 3.  Implement Remainder of Peano numbers. --------------
; Remainder of Peano numbers
; input-contract: (and (nat? M) (nat? N) (not (zero? N)))
; output-contract: (nat? (rem M N)), that is (rem M N) is the pnum representation of m%n
; Returns a Peano number representing the remainder r of m divided by n.
; Remember for x/y, the quotient q and remainder r meet the following requirement: x = q*y + r with 0 <= r < x.
; Requirements: You may not convert to or from traditional integers, nor use length. Your function must be recursive.
(define (rem M N)
  (if (ltnat? M N) M
      (rem (sub M N) N)))

;Test Bed
(display "Question 3 - Remainder (10 points)\n")
(define-test-suite peano-rem
  (check-equal? (rem ten three) one)
  (check-equal? (rem ten two) zero)
  (check-equal? (rem nine three) zero)
  (check-equal? (rem nine one) zero)
  (check-equal? (rem eight five) three)
  (check-equal? (rem eight four) zero)
  (check-equal? (rem one seven) one)
  (check-equal? (rem seven five) two)
  (check-equal? (rem six six) zero)
  (check-equal? (rem six two) zero)
)
(define q3_score  (- 10 (run-tests peano-rem 'verbose)))

;--------------- Question 4.  Implement Not Equal of Peano numbers. --------------
; Not Equal of Peano numbers
; input-contract: (and (nat? M) (nat? N))
; output-contract: (boolean? (neq M N)). (neq M N) is the predicate for m ≠ n
;                  in other words, returns true when the numbers are not equal and false when they are equal
; Requirements: You may not convert to or from traditional integers, nor use length. Your function must be recursive.
(define (neq M N)
  (cond
    [(and (zero? M) (zero? N)) #f]
    [(zero? N) #t]
    [(zero? M) #t]
    [else (neq (pred M)(pred N))]))

;Test Bed
(display "Question 4 - Not Equal (10 points)\n")
(define-test-suite peano-neq
  (check-equal? (neq ten ten) #f)
  (check-equal? (neq six six) #f)
  (check-equal? (neq five five) #f)
  (check-equal? (neq four four) #f)
  (check-equal? (neq three three) #f)
  (check-equal? (neq two two) #f)
  (check-equal? (neq one one) #f)
  (check-equal? (neq seven five) #t)
  (check-equal? (neq six nine) #t)
  (check-equal? (neq six two) #t)
)
(define q4_score  (- 10 (run-tests peano-neq 'verbose)))

;--------------- Question 5.  Implement GCD of Peano numbers. --------------
#|
             Implement a function to compute the greatest common divisor
             of the Peano numbers m and n.  g = gcd(m,n) satisfies
             1)  g is a common divisor of m and n.
                 g divides m and g divides n.  I.E. the remainder when
                 dividing m and n by g is 0.
             2)  g is the greatest common divisor.
                 If e divides m and e divides n then e must divide g.

             The gcd(m,n) can be computed recursively.
             1)  gcd(m,0) = m
             2)  gcd(m,n) = gcd(n,remainder of m divided by n).
|#

; Greatest common divisor of Peano numbers
; input-contract: (and (nat? M) (nat? N)))
; output-contract: (nat? (gcd M N)). i.e. (gcd M N) is the pnum representation for gcd(m,n)
; Requirements: You may not convert to or from traditional integers, nor use length. Your function must be recursive.
; HINT:  See algorithm in comments above
(define (gcd M N)
  (if (zero? N) M
      (gcd N (rem M N))))


(display "Question 5 - GCD (20 points)\n")
(define-test-suite peano-gcd
  (check-equal? (gcd two ten) two)
  (check-equal? (gcd two four) two)
  (check-equal? (gcd three zero) three)
  (check-equal? (gcd three two) one)
  (check-equal? (gcd three three) three)
  (check-equal? (gcd three five) one)
  (check-equal? (gcd three six) three)
  (check-equal? (gcd three nine) three)
  (check-equal? (gcd three ten) one)
  (check-equal? (gcd four two) two)
  (check-equal? (gcd five ten) five)
  (check-equal? (gcd six one) one)
  (check-equal? (gcd six seven) one)
  (check-equal? (gcd seven six) one)
  (check-equal? (gcd eight two) two)
  (check-equal? (gcd eight four) four)
  (check-equal? (gcd eight eight) eight)
  (check-equal? (gcd nine one) one)
  (check-equal? (gcd nine ten) one)
  (check-equal? (gcd ten three) one)

)
(define q5_score  (- 20 (run-tests peano-gcd 'verbose)))

;--------------- Question 6.  Implement Mod Power of Peano numbers. --------------
#|
            The mod power is commonly used in cryptography.
            We want to compute (m^n) % b
            When we compute exponents, the number normally get very large.
            If we only need the remainder, we can take it at each stage
            This keeps our numbers smaller

            The algorithm is described below
            (m^n)%b = 1 if n==0
            (m^n)%b = (* m (m^(n-1)) %b otherwise
|#

; Mod Power of Peano numbers
; input-contract: (and (nat? M) (nat? N) (nat? B))
; output-contract: (nat? (modpow M N B)), where (modpow M N B) is the pnum representation for (m^n)% b
; Requirements: You may not convert to or from traditional integers, nor use length. Your function must be recursive.
; HINT:  See algorithm in comments above
(define (modpow M N B)
  (if (zero? N) (succ zero)
      (rem (mult M (modpow M (pred N) B)) B)))

(display "Question 6 - Mod Power (20 points)\n")
(define-test-suite peano-modpow
  (check-equal? (modpow two zero three) one)
  (check-equal? (modpow two one three) two)
  (check-equal? (modpow two two three) one)
  (check-equal? (modpow two three three) two)
  (check-equal? (modpow three zero four) one)
  (check-equal? (modpow three one four) three)
  (check-equal? (modpow three two four) one)
  (check-equal? (modpow three three four) three)
  (check-equal? (modpow two zero four) one)
  (check-equal? (modpow two one four) two)
  (check-equal? (modpow two two four) zero)
  (check-equal? (modpow two three four) zero)
  (check-equal? (modpow three zero five) one)
  (check-equal? (modpow three one five) three)
  (check-equal? (modpow three two five) four)
  (check-equal? (modpow three three five) two)
  (check-equal? (modpow three four five) one)
  (check-equal? (modpow three five five) three)
  (check-equal? (modpow three four two) one)
  (check-equal? (modpow three five two) one)
)
(define q6_score  (- 20 (run-tests peano-modpow 'verbose)))

;--------------- Question 7.  Inductive Proof --------------

;This question will be manually graded by the course assistants

;Prove the following claim:
;let m,n be any positive integers.
;Prove the claim that (ltnat? (toPeano m) (toPeano (+ m n))) = #t

;You may use the following in your proof [for a bonus 10pts to this assignment, you can prove the lemmas.
;Note: proofs of the lemmas are short and do NOT require induction; a standard equational reasoning argument suffices]
;Property of Addition: (+ (+ x y) 1) = (+ (+ x 1) y)
;Lemma 1: (zero? (toPeano (+ x y))) = #f if x and y are nonzero
;Lemma 2: (pred (toPeano (+ x 1))) = (toPeano x)


;Provide a Proof by Induction on m assuming n is a constant.
;This question is worth 20 points. To get full credit, you must show all the steps and their justifications.

#| Enter proof here; you may create extra space within this comment block as needed.

; 1 point: First, explain what the claim is saying in plain English (you should only need a single sentence;
;do NOT refer to any details of any code. Imagine you are talking to someone that is not a programmer).
; For any positive integer numbers m and n, the Peano number m is less than the Peano number m + n

;Anchor Identified: 1 point
; anchored at m = 0 and n>=1

;Base Case LHS: 4 points
;(ltnat? (toPeano 0) (toPeano (+ 0 n))); Premise of Base Case LHS
;(ltnat? (toPeano 0) (toPeano n))); Math
;(ltnat? (if (= 0 0) null...)(if (= n 0) null (cons 's (toPeano (- n 1))))); Apply def of toPeano
;(ltnat? (if #t null..)(if #f nul..)); Eval equal
;(ltnat? null (cons 's (toPeano (- n 1))));Eval if
;(ltnat? null (toPeano (+ (- n 1) 1))); Eval cons
;(ltnal? null (toPeano n)); Math
;(if (zero? (toPeano n) #f (if (zero? null) #t (ltnam? ...)))); Def. of ltnam
;(if #f #f (if (zero? null #t (ltnam?....)))); Eval zero
;(if(zero? null #t (ltnam?...))); Eval if
;(if #t #t (ltnam?...)); Eval zero
;#t; Eval if
            
;Base Case RHS: 1 points
;#t ; Premise of Base Case RHS
;Since LHS = RHS, the base case is established

;Inductive Hypothesis: 2 points
;IH: Assume that for a value k we have (ltnat? (toPeano k) (toPeano (+ k n))) = #t

;Leap Case LHS: 10 points
;(ltnat? (toPeano (+ k 1)) (toPeano (+ (+ k 1) n))); Premise of Leap case LHS
;(ltnat? (if (= (+ k 1) 0) null (cons 's (toPeano (- (+ k 1) 1)...)))); Def of toPeano
;(ltnal? (if #f null ...)); Eval equal
;(ltnal? (cons 's (toPeano (- (+ k 1) 1)...); Eval if
;(ltnal? (cons 's (toPeano k) ...); Math
;(ltnal? (cons 's (toPeano k)(if (= (+ (+ k 1) n) 0) null (cons 's (toPeano (- (+ (+ k 1) n) 1)))))); Def of toPeano
;(ltnal? (cons 's (toPeano k) (if  #f null...); Eval equal
;(ltnal? (cons 's (toPeano k) (cons 's (toPeano (- (+ (+ k 1) n) 1))))); Eval if
;(ltnal? (cons 's (toPeano k)(cons 's (toPeano (+ k n))))); Math
;(ltnal? (toPeano (+ k 1)) (toPeano (+ (+ k n) 1))); Eval cons
;(if (zero? (toPeano (+ (+ k n) 1....); Def of ltnal
;(if #f #f ....); By Lemma 1
;(if (zero? (toPeano (+ k 1) #t...); Eval if
;(if #f #t...); By Lemma 1
;(ltnam? (pred (toPeano (+ k 1) (pred (toPeano (+ (+ k n 1))))))); Eval if
;(ltnam? (toPeano k) (toPeano (+ k n))); By Lemma 2
;#t ; Invoke IH                    

;Leap Case RHS: 1 points
; #t; Premise of Leap Case RHS
;Since LHS = RHS, the leap case is established
;Both the base case and leap case have been demonstrated, thus by POMI, we have (ltnat? (toPeano m) (toPeano (+ m n))) = #t


|#
;;;;;;;;;;;;;;Grade Summary;;;;;;;;;;;;;;;;;;;;;;;
(display "------Grade Summary------\n")
(display "Q1 Scored: ")(display q1_score)(display "/10\n")
(display "Q2 Scored: ")(display q2_score)(display "/10\n")
(display "Q3 Scored: ")(display q3_score)(display "/10\n")
(display "Q4 Scored: ")(display q4_score)(display "/10\n")
(display "Q5 Scored: ")(display q5_score)(display "/20\n")
(display "Q6 Scored: ")(display q6_score)(display "/20\n")
(display "Q7 Scored: ")(display 0)(display "/20\n")
(define grand_total (+ q1_score q2_score q3_score q4_score q5_score q6_score))
(display "\n")(display "Total: ")(display grand_total)(display "/100\n")