#lang racket

;The following two lines are required to test the code.
(require rackunit)
(require rackunit/text-ui)


; input-spec L is a list (possibly nested list)
; pair is a list with two members (A B)
; where A and B are expressions or values.  
; output-spec  Each occurence of A that appears anywhere in L, at any depth, gets replaced by B
; example: if L is the list '(x (y) z), then (sub L '(y 5)) would yield the list '(x (5) z)

#|  Hoang comments:
    This function recurrence works by   adding the first item to the result if that item is not the first element in the pair
                                        adding the subsitution to the result if that item is the first element in the pair
                                        adding the sub-ed rest (recursive) onto the result
|#
(define (sub L pair)
   (if (null? pair)
       L
       (if (null? L)
           '()
           (if (equal? (first L) (first pair))
               (cons
                    (second pair)
                    (sub (rest L) pair)
                )
               (cons (first L) (sub (rest L) pair))
            
            )
        )
    )
)

(display "Testing function: sub\n")
(define-test-suite test_sub
  (check-equal? (sub '(1 2 3) '(1 2) ) '(2 2 3))
  (check-equal? (sub '(1 2 1 3) '(1 2) ) '(2 2 2 3))
  (check-equal? (sub '(1 2 1 3) '() ) '(1 2 1 3))
  (check-equal? (sub '() '(1 2) ) '())
  (check-equal? (sub '() '() ) '())
  (check-equal? (sub '(1 2 -1 3) '(1 2) ) '(2 2 -1 3))
  (check-equal? (sub '(1 2 -1 3) '(5 -1) ) '(1 2 -1 3))
  (check-equal? (sub '(a b c) '(a d) ) '(d b c))
)
(run-tests test_sub 'verbose)
;input-spec L1 is a list (possibly nested list)
; substitution1 and substitution2 are both pairs (A1 B1) and (A2 B2)
; where Ai and Bi are expressions or values.  
;output-spec  first substitutions are done of B1 for A1.  Then in that
; result substitutions are done of B2 for A2.  So if A2 is the same as B1
; then two changes might happen.
(define (sequentialSub2 L1 substitution1 substitution2)
  (sub (sub L1 substitution1) substitution2)
  )

(display "Testing function: sequentialSub2\n")
(define-test-suite test_sub2
  (check-equal? (sequentialSub2 '(1 3 (1 5) 3) '(3 (1 5)) '((1 5) 17)) '(1 17 17 17))
  (check-not-equal? (sequentialSub2 '(1 3 (1 5) 3) '(3 (1 5)) '((1 5) 17)) '(1 (1 5)  17 (1 5)))
  (check-equal? (sequentialSub2 '(1 3 (1 5) (3)) '(3 (1 5)) '((1 5) 17)) '(1 17 17 (3)))
  (check-equal? (sequentialSub2 '(1 3 (1 5) 3) '(3 (1 5)) '()) '(1 (1 5) (1 5) (1 5)))
  (check-equal? (sequentialSub2 '(1 3 (1 5) 3) '() '((1 5) 17)) '(1 3 17 3))
  (check-equal? (sequentialSub2 '() '(3 (1 5)) '((1 5) 17)) '())
  (check-equal? (sequentialSub2 '() '() '((1 5) 17)) '())
  (check-equal? (sequentialSub2 '() '() '()) '())
)
(run-tests test_sub2 'verbose)

;The following function takes us five lines to code, not including comments.

; input-spec  L is an arbitrary list or possibly an atom.
; reps (short for "replacements") is a list of pairs
; output-spec result is L but with each pair in the replacement list
; substituted in turn.  So if one pair makes something
; that a later pair applies to, that subsequent pair will be substituted too.

(define  (sequentialSubMulti L substitutions)
  (if (null? substitutions)
      L
      (sequentialSubMulti  (sub  L (first substitutions)) (rest substitutions))
   )
 )

(display "Testing function: sequentialSubMulti\n")
(define-test-suite test_sequentialSubMulti
  (check-equal? (sequentialSubMulti '(a b (c (d (x y b z (b)) q))) '((b 42))) '(a 42 (c (d (x y b z (b)) q))))
  (check-equal? (sequentialSubMulti '(a b (c (d (x y b z (b)) q))) '((b 42) '())) '(a 42 (c (d (x y b z (b)) q))))
  (check-equal? (sequentialSubMulti '(a b (c (d (x y b z (b)) q))) '('() (b 42) '())) '(a 42 (c (d (x y b z (b)) q))))
  (check-equal? (sequentialSubMulti '(a b (c (d (x y b z (b)) q))) '('() (b 42) '() '(g x))) '(a 42 (c (d (x y b z (b)) q))))
  (check-equal? (sequentialSubMulti
                        '(1 (1 3 ) (1 3 5) (1 3 (1 3) (((1 3))))) '( ((1 3) 47) (3 48) ( (1 48 5) 61))) '(1 47 (1 3 5) (1 3 (1 3) (((1 3))))) )
  ;; need more tests!  And to check that the above are correct!'( (1 5) 2) ) '(1 2 (1 5 (3 2 (1 2 3) 2)) 3))
     )
(run-tests test_sequentialSubMulti 'verbose)



;input-spec L1 is a list (possibly nested list)
; substitutions is a list of pairs ( (A1 B1) (A2 B2) ... (An Bn)
; where Ai and Bi are expressions or values.  
;output-spec  Each specified substitution is applied to the result
; of the previous substitution.  So for each Aj  the same as some Bi for
;i<j, multiple changes might happen.

(define (subR L pair)
   (if (null? pair)
       L
       (if (null? L)
           '()
           (if (equal? (first L) (first pair))
               (cons
                (second pair)
                (subR (rest L) pair)
                )
               (if (list? (first L))
                   (append
                    (cons
                     (subR(first L) pair)
                     '()
                     )
                    (subR (rest L) pair)
                    )
                   (cons (first L) (subR (rest L) pair))
                   )
              )
         )
    )
)

(display "Testing function: subR\n")
(define-test-suite test_subR
  (check-equal? (subR '(1 2 3) '(1 2) ) '(2 2 3))
  (check-equal? (subR '(1 2 1 3) '(1 2) ) '(2 2 2 3))
  (check-equal? (subR '(a b (c (d (x (b) y b z) q))) '(b 42)) '(a 42 (c (d (x (42) y 42 z) q))))
  (check-equal? (subR '(3 (1 5) 3) '(3 (1 5))) '((1 5) (1 5) (1 5)))
  (check-equal? (subR '() '(1 3)) '())
  (check-equal? (subR '(a b (c (d (x (b) y b z) q))) '()) '(a b (c (d (x (b) y b z) q))))

)
(run-tests test_subR 'verbose)

; input-spec  L1            is an arbitrary list or possibly an atom.
;             substitutions is a list of substitution pairs
; output-spec result        is L1 with each Substitutement done deep and recursively
;                               each substitution done in order of substitutions list
;                               if one Substitutement makes something that a subsequent Substitutement applies to, that will be done too.
;                             

(define  (sequentialSubRMulti L substitutions)
   (if (null? substitutions)
      L
      (sequentialSubRMulti  (subR  L (first substitutions)) (rest substitutions))
   )

  )

(display "Testing function: sequentialSubRMulti\n")
(define-test-suite test_sequentialSubRMulti
         (check-equal? (sequentialSubRMulti '(a b (c (d (x y b z (b)) q))) '((b 42))) '(a 42 (c (d (x y 42 z (42)) q))))
         (check-equal? (sequentialSubRMulti
                        '(1 (1 3 ) (1 3 5) (1 3 (1 3) (((1 3))))) '( ((1 3) 47) (3 48) ( (1 48 5) 61))) '(1 47 61 (1 48 47 ((47)))))
         (check-equal? (sequentialSubRMulti
                 '(1 (1 3 ) (1 3 5) (1 3 (1 3) (((1 3))))) '( '() ((1 3) 47) (3 48) ( (1 48 5) 61))) '(1 47 61 (1 48 47 ((47)))))
         (check-equal? (sequentialSubRMulti
                 '(1 (1 3 ) (1 3 5) (1 3 (1 3) (((1 3))))) '( ((1 3) 47) '() (3 48) ( (1 48 5) 61))) '(1 47 61 (1 48 47 ((47)))))
         (check-equal? (sequentialSubRMulti
                 '(1 (1 3 ) (1 3 5) (1 3 (1 3) (((1 3))))) '( ((1 3) 47) (3 48) ( (1 48 5) 61) '())) '(1 47 61 (1 48 47 ((47)))))
         (check-equal? (sequentialSubRMulti
                 '(1 (1 3 ) (1 3 5) (1 3 (1 3) (((1 3))))) '()) '(1 (1 3 ) (1 3 5) (1 3 (1 3) (((1 3))))))
         (check-equal? (sequentialSubRMulti
                 '() '( ((1 3) 47) (3 48) ( (1 48 5) 61))) '())
        ;; need more tests!  And to check that the above are correct!
  
  )
(run-tests test_sequentialSubRMulti 'verbose)

