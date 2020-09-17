#lang racket

#|
CS 270 Math Foundations of CS
Spring 2019
Instructor:  Profs. Bruce Char, Jeremy Johnson and Mark Boady

Submit in BBLearn.

Submit a single .rkt file with no spaces in the filename.
Each of the following six questions are worth 6 points each

Assignment 3
Recursive definitions and functions

In this assignment students will write several functions that
recursively process numbers.  In class we provided two recursive
definitions of numbers.  The recursive definitions provide a set
of constructors which capture the ways to construct a number.
Peano numbers have two constructors, one of which, succ, is recursive,
and Binary Numbers have three constructors, two of which, double and
double-plus1 are recursive.

1)  Peano arithmetic

    In words:  A number is either zero, or, recursively, the successor
    of a number is a number.

    Formally:  Number := zero|(succ Number)

2)  Binary arithmetic

    In words:  A binary number is either zero, or recursively, doubling
    a number or doubling a number and adding one gives a binary number.

    Formally:  BinNumber := zero|(double BinNumber)|(double-plus1 BinNumber)

    We can interpret binary numbers by assigning zero to the number zero
    and if b is the value of a binary number than (double b) has value 2*b
    and (double-plus1 b) has value 2*b+1.

    Note that there is more than one way to construct binary numbers with
    the same value.  E.G.  (double zero) has the same value as zero.
    A binary number is normalized if (double zero) is recursively replaced
    by zero, i.e. trailing zeros are removed.

    When recursively processing either types of numbers, you must
    handle cases corresponding to the different constructors.  Recursive
    calls must have inputs whose size is smaller, where size is the
    number of constructors needed to build the number.

    In the first part of this assignment you will write recursive
    functions to subtract and divide Peano numbers.  You will also
    implement the Euclidean algorithm to compute the greatest common
    divisor, gcd, of Peano numbers.

    In the second part of this assignment you will write recursive functions
    to add and multiply Binary numbers using the recursive definition above.

|#

(require racket/contract)
(require rackunit)
(require rackunit/text-ui)

#|

Part I.  Peano arithmetic.

Question 1.  Implement subtraction of Peano numbers.  See specification
             below.

Question 2 & 3.  Implement division of Peano numbers.  Implement functions to
             compute the quotient and remainder when dividing Peano numbers
             m and n.  I.E. compute q the quotient and r the remainder
             such that m = q*n + r with 0 <= r < n.  See specification
             below.

             Your division and remainder functions should work directly with
             Peano-style representations.  While it would be possible to
             implement these operations by constructing conversion functions that
             convert a number to and from decimal notation, then using the built-in
             Racket numerical operations,  you should not use this path since it doesn't show that you understand how to
             perform those operations on your own.

Question 4.  Implement a function to compute the greatest common divisor
             of the Peano numbers m and n.  g = gcd(m,n) satisfies
             1)  g is a common divisor of m and n.
                 g divides m and g divides n.  I.E. the remainder when
                 dividing m and n by g is 0.
             2)  g is the greatest common divisor.
                 If e divides m and e divides n then e must divide g.

             The standard algorithm for computing the gcd, the famous
             Euclidean algorithm can be implemented either with a loop or
             recursively  The recursive version uses this reasoning:
             1)  gcd(m,0) = m
             2)  gcd(m,n) = gcd(n,remainder of m divided by n).

             Your implementation of the gcd should use your Peano-arithmetic
             implementations from questions 2 and 3.
|#

(define (zero? n)
  (equal? n 'zero))

(define (nat? x)
  (cond
    [(zero? x) #t]
    [(pair? x) (and (eq? (first x) 'succ) (nat? (second x)))]
    [else #f]))

(define (succ n)
  (list 'succ n))

(define (pred n)
  (if (zero? n) 'zero (second n)))

(define zero 'zero)
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


; addition of Peano numbers
; Input: m, n Peano numbers
; Output: a Peano number whose value is m+n
(define (plus m n)
  (if (zero? m)
      n
      (succ (plus (pred m) n))))

; multiplication of Peano numbers
; Input: m, n Peano numbers
; Output: a Peano number whose value is m*n
(define (mult m n)
  (if (equal? m 'zero)
      'zero
      (plus n (mult (pred m) n))))
; comparison of Peano numbers
; Input: m, n Peano numbers
; Output: a boolean = #t if the value of m < value of n and #f otherwise
(define (ltnat? m n)
  (cond
    [(zero? n) #f]
    [(zero? m) #t]
    [else (ltnat? (pred m) (pred n))]))

; subtraction of Peano numbers
; Input: m, n Peano numbers
; Output: a Peano number whose value is m-n if m >= n.
;         It is undefined otherwise.
#| Hoang's comment
       Second m by n times to substract n from m
|#
(define (sub m n)
    (if (zero? n)
        m
        (second(sub m (pred n) ))))


; Division of Peano numbers
; Input: m, n Peano numbers
; Output: a Peano number whose value q is the quotient of m divided by n.
;         m = q*n + r with 0 <= r < n.
(define (div m n)
    (if (ltnat? m n)
        zero
        (succ (div (sub m n) n))
    )
)

; Remainder of Peano numbers
; Input: m, n Peano numbers
; Output: a Peano number whose value r is the remainder of m divided by n.
;         m = q*n + r with 0 <= r < n.
(define (rem m n)
    (if (ltnat? m n)
        m
        (rem (sub m n) n)
    )
)

; Greatest common divisor of Peano numbers
; Input: m, n Peano numbers
; Output: a Peano number equal to gcd(m,n).
; Note:  See algorithm for GCD near top of file
;             1)  gcd(m,0) = m
;             2)  gcd(m,n) = gcd(n,remainder of m divided by n).
(define (gcd m n)
    (if (zero? n)
        m
        (gcd n (rem m n))
    )
)

(display "Question 1 - Subtraction (15 points)\n")
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
  (check-equal? (sub five three) two)
  (check-equal? (sub four two) two)
  (check-equal? (sub three two) one)
  (check-equal? (sub two one) one)
  (check-equal? (sub one zero) one)
)
(run-tests peano-subtract 'verbose)

(display "Question 2 - Division (15 points)\n")
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
  (check-equal? (div five three) one)
  (check-equal? (div four two) two)
  (check-equal? (div three two) one)
  (check-equal? (div two one) two)
  (check-equal? (div one five) zero)
)
(run-tests peano-div 'verbose)

(display "Question 3 - Remainder (15 points)\n")
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
  (check-equal? (rem five three) two)
  (check-equal? (rem four two) zero)
  (check-equal? (rem three two) one)
  (check-equal? (rem two one) zero)
  (check-equal? (rem one five) one)
)
(run-tests peano-rem 'verbose)

(display "Question 4 - GCD (15 points)\n")
(define-test-suite peano-gcd
  (check-equal? (gcd nine ten) one)
  (check-equal? (gcd three nine) three)
  (check-equal? (gcd six seven) one)
  (check-equal? (gcd eight two) two)
  (check-equal? (gcd nine one) one)
  (check-equal? (gcd ten three) one)
  (check-equal? (gcd six seven) one)
  (check-equal? (gcd five ten) five)
  (check-equal? (gcd three zero) three)
  (check-equal? (gcd six one) one)
  (check-equal? (gcd eight four) four)
  (check-equal? (gcd eight eight) eight)
  (check-equal? (gcd three two) one)
  (check-equal? (gcd three five) one)
  (check-equal? (gcd three three) three)
)
(run-tests peano-gcd 'verbose)
#|

Part II.  Binary arithmetic.

Question 5.  Implement a recursive function to add two binary numbers
(non-negative integers using the D/DP1 representation).  Your
implementation should work directly on the D/DP1/zero lists that
represent the numbers.  The number of recursive calls should be at
most to the number of symbols in the lists.  For example, in lab 4,
you discovered that (binadd (D A) (D B)) is always (regardless of what
A and B are) (D (binadd A B)).  So if your binadd function
implementation is processing its two operands and finds that they are
both lists whose first element is D, you can implement the addition in
that case as one recursive call to binadd, not (D B) or (D A)
recursive calls involving inc or dec.  Such efficient processing will
process numbers one symbol at a time.  Since there are only
log-base-two(N)+1 symbols needed to represent the number N (for N>= 1,
or 1 symbol to represent zero), this is a more efficient way of doing
the addition.  For example, consider the number N= 1024.  Only eleven
symbols (not counting parentheses) are needed to represent N in D/DP1
notation, so processing N when it is in that form should take only 11
and not 1024 recursive calls.

Question 6.  Implement a recursive function to multiply two binary
numbers (non-negative integers working directly on the D/DP1
representation).  If you are doing things properly you won't need any
calls to dec or inc, just binadd and recursive calls to binmult and
basic list operations.
            
|#

(define binzero 'zero)

(define (binzero? b)
  (eq? b binzero))

(define (double b)
  (list 'D b))

(define (double-plus1 b)
  (list 'DP1 b))

(define (double? b)
  (cond
    [(not (pair? b)) #f]
    [(eq? (first b) 'D) #t]
    [else #f]))

(define (double-plus1? b)
  (cond
    [(not (pair? b)) #f]
    [(eq? (first b) 'DP1) #t]
    [else #f]))

(define (op b)
  (second b))

(define (binone? b)
  (equal? b binone))

(define binone (double-plus1 binzero))
(define bintwo (double binone))
(define binthree (double-plus1 binone))
(define binfour (double bintwo))
(define binfive (double-plus1 bintwo))
(define binsix (double binthree))
(define binseven (double-plus1 binthree))
(define bineight (double binfour))
(define binnine (double-plus1 binfour))
(define binten (double binfive))

; increment a binary number
; Inputs: a binary number b
; Output: a binary whose value is the value of b + 1.
;         if b is normalized (inc b) will be normalized.
(define(inc b)
  (cond
    [(binzero? b) (double-plus1 b)]
    [(double? b) (double-plus1 (op b))]
    [(double-plus1? b) (double (inc (op b)))]))

; add two binary numbers
; Inputs: a and b binary numbers.
; Output: a binary whose value is the value of a + b.
;         if a and b are normalized (plus a b) will be normalized.
(define (binplus a b)
    (cond
        [(binzero? a) b]
        [(binzero? b) a]
        [(and (double-plus1? a) (double-plus1? b))
             (double (binplus (op a) (inc (op b))))]
        [(and (double-plus1? a) (double? b))
             (double-plus1 (binplus (op a) (op b)))]
        [(and (double? a) (double? b))
             (double (binplus (op a) (op b)))]
        [(and (double? a) (double-plus1? b))
             (double-plus1 (binplus (op a) (op b)))]
    )
)

(display "Question 5 - Binary Plus (20 points)\n")
(define-test-suite bin-plus-test
  (check-equal? (binplus binzero binone) binone)
  (check-equal? (binplus binone bintwo) binthree)
  (check-equal? (binplus bintwo binsix) bineight)
  (check-equal? (binplus binthree bintwo) binfive)
  (check-equal? (binplus binfour binfive) binnine)
  (check-equal? (binplus binfive bintwo) binseven)
  (check-equal? (binplus binsix binone) binseven)
  (check-equal? (binplus binseven bintwo) binnine)
  (check-equal? (binplus bineight bintwo) binten)
  (check-equal? (binplus binnine binzero) binnine)
  (check-equal? (binplus binten binzero) binten)
  (check-equal? (binplus binnine binone) binten)
  (check-equal? (binplus bineight binzero) bineight)
  (check-equal? (binplus binseven binone) bineight)
  (check-equal? (binplus binsix binthree) binnine)
  (check-equal? (binplus binfive binthree) bineight)
  (check-equal? (binplus binfour bintwo) binsix)
  (check-equal? (binplus binthree binseven) binten)
  (check-equal? (binplus bintwo bintwo) binfour)
  (check-equal? (binplus binone binfive) binsix)
)
(run-tests bin-plus-test 'verbose)
  
; multiply two binary numbers
; Inputs: a and b binary numbers.
; Output: a binary whose value is the value of a * b.
;         if a and b are normalized (mult a b) will be normalized.
; Note:  in order to guarantee that the result is normalized, you
;        must handle the case when a or b is one separately.  To
;        see why, compute (mult one one)

(define (binmult a b)
    (cond
        [(binone? a) b]
        [(binone? b) a]
        [(binzero? a) binzero]
        [(binzero? b) binzero]
        [(and (double-plus1? b))
             (binplus (double (binmult a (second b))) a)]
        [else
             (double (binmult a (second b)))]
       
  )
)

(display "Question 6 - Binary Multiply (20 points)\n")
(define-test-suite bin-mult-test
  (check-equal? (binmult binzero binone) binzero)
  (check-equal? (binmult binone binzero) binzero)
  (check-equal? (binmult binzero binthree) binzero)
  (check-equal? (binmult binnine binzero) binzero)
  (check-equal? (binmult binone binthree) binthree)
  (check-equal? (binmult binone binseven) binseven)
  (check-equal? (binmult binfive binone) binfive)
  (check-equal? (binmult binten binone) binten)
  (check-equal? (binmult bintwo binone) bintwo)
  (check-equal? (binmult bintwo bintwo) binfour)
  (check-equal? (binmult bintwo binthree) binsix)
  (check-equal? (binmult bintwo binfour) bineight)
  (check-equal? (binmult bintwo binfive) binten)
  (check-equal? (binmult binthree bintwo) binsix)
  (check-equal? (binmult binthree binthree) binnine)
  (check-equal? (binmult binfour bintwo) bineight)
  (check-equal? (binmult binfive bintwo) binten)
  (check-equal? (binmult binfive binfour) (double binten))
  (check-equal? (binmult binfour binthree) (double binsix))
  (check-equal? (binmult binfive binthree) (double-plus1 binseven))
)
(run-tests bin-mult-test 'verbose)
