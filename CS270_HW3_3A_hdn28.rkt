#lang racket

(require rackunit)

#|
Hoang's Comment
    Racket cases for sequential substitutions functions has been in the last HW
    This one I will focus on writing cases for parallel substitutions
|#
;input-spec L is a list (possibly nested list)
; substitution is a pair (A1 B1) where A1 and B1 are expressions or values
;output-spec  every item of L which is equal (matches
; to A1 has B1 instead in the result.  Racket doesn't change list
; it creates a new structure where values in it are either copied over from
; the input L, or are substitute values.

; [6 pts]
(define (substitute L substitution)
(if (null? substitution)
       L
       (if (null? L)
           '()
           (if (equal? (first L) (first substitution))
               (cons
                    (second substitution)
                    (substitute (rest L) substitution)
                )
               (cons (first L) (substitute (rest L) substitution))
            
            )
        )
    )
)
(module+ test
  (require rackunit)
  (check-equal? (substitute '(1 2 3) '(2 7)) '(1 7 3) "basic one item substitution of a value")
  (check-not-equal? (substitute '() '(2 7)) '(1 7 3) "basic one item substitution of a value")
  (check-equal? (substitute '(a b c) '(c (7 8 ))) '(a b (7 8)) "basic one item substitution of a list")
  (check-equal? (substitute '(c a b c) '(c (7 8 ))) '((7 8) a b (7 8)) "basic one item substitution of a list, more than once")
  ;(check-exn exn:fail:contract? (substitute '(c a b c) '()))
  )


;input-spec L is a list (possibly nested list)
; substitution1 and substitution2 are both pairs (A1 B1) and (A2 B2)
; where Ai and Bi are expressions or values.  
;output-spec  first substitutions are done of B1 for A1.  Then in that
; result substitutions are done of B2 for A2.  So if A2 is the same as B1
; then two changes might happen.

(define (sequentialSubstitute2 L substitution1 substitution2)
  (substitute (substitute L substitution1) substitution2))

(module+ test
  (check-equal? (sequentialSubstitute2 '(1 3 (1 5) 3) '(3 (1 5)) '((1 5) 17)) '(1 17 17 17))
  (check-not-equal? (sequentialSubstitute2 '(1 3 (1 5) 3) '(3 (1 5)) '((1 5) 17)) '(1 (1 5)  17 (1 5)))
  ;(check-exn exn:fail:contract? (sequentialSubstitute2 '(1 3 (1 5) 3) '() '() ) '(1 3 (1 5) 3))
  )

;input-spec L is a list (possibly nested list)
; substitutions is a list of pairs ( (A1 B1) (A2 B2) ... (An Bn)
; where Ai and Bi are expressions or values.  
;output-spec  Each specified substitution is applied to the result
; of the previous substitution.  So for each Aj  the same as some Bi for
;i<j, multiple changes might happen.

; [6 pts]
(define (sequentialSubstitute L substitutions)
  (if (null? substitutions)
      L
      (sequentialSubstitute  (substitute  L (first substitutions)) (rest substitutions))
   )
 )

(module+ test
  (check-equal? (sequentialSubstitute '(1 2 3) '( ( 1 2) (2 3) (3 4))) '(4 4 4))
  (check-not-equal? (sequentialSubstitute '(1 2 3) '( ( 1 2) (2 3) (3 4))) '(2 3 4))
  )

;input-spec L is a list (possibly nested list)
; substitution is a pair (A B) whre A and B are lists or values.
;output-spec  Each item in the list is checked to see if it matches A
;  If it does, then the result has B replacing that item.
;  If it does not match a, then if the item is a list, then it is inspected
;  recursively, so that if the item is a list one of whose items is A, then
;  that is replaced by B in the result.  The inspection continues
; recursively for sublists, subsublists, etc. of the item.  The
;  base case of the recursion is when the item is an atomic value (something not a list, such
; as a number, string, or boolean.


(define (sequentialSubstituteR L substitution)
  ; works even through recursing through subexpressions of L
  (if (null? L) L
      (if (list? L)
          (if (equal? (first L) (first substitution))
                      (cons (second substitution) (sequentialSubstituteR (rest L) substitution))
                      (cons (sequentialSubstituteR (first L) substitution) (sequentialSubstituteR (rest L) substitution)))
          (if  (equal? L (first substitution)) ; not a list so L is an atom
                   (second substitution)
                   L ))))
(module+ test
  (check-equal? (sequentialSubstituteR '(1 2 3) '( 1 2) ) '(2 2 3))
  (check-equal? (sequentialSubstituteR '(1 2 1 3) '( 1 2) ) '(2 2 2 3))
  (check-equal?  (sequentialSubstituteR '(1 2 (1 5) 3) '( 1 2) ) '(2 2 (2 5) 3))
  (check-equal?  (sequentialSubstituteR '(1 2 (1 5 (3 1 7)) 3) '( 1 2) ) '(2 2 (2 5 (3 2 7)) 3))
  (check-equal?  (sequentialSubstituteR '(1 2 (1 5) 3) '( (1 5)  2) ) '(1 2 2 3))
  (check-equal?  (sequentialSubstituteR '(1 (1 5) (1 5 (3 (1 5)  (1 (1 5) 3) (1 5) )) 3) '( (1 5) 2) ) '(1 2 (1 5 (3 2 (1 2 3) 2)) 3))
  ; needs more tests
  )

; input-spec  L is an arbitrary list or possibly an atom.
; substitutions is a list of substitution pairs
; output-spec result is L with each Substitutement in substitutions list
; tried and performed in turn.  So if one Substitutement makes something
; that a subsequent Substitutement applies to, that will be done too.

; [6 pts]
(define  (sequentialSubstituteRMulti L substitutions)
  (if (null? substitutions)
      L
      (sequentialSubstituteRMulti  (sequentialSubstituteR  L (first substitutions)) (rest substitutions))
  )
)

(module+ test
         (check-equal? (sequentialSubstituteRMulti
                        '(1 (1 3 ) (1 3 5) (1 3 (1 3) (((1 3))))) '( ((1 3) 47) (3 48) ( (1 48 5) 61))) '(1 47 61 (1 48 47 ((47)))) )
  ; need more tests!
         )
;input-spec L is a list possibly nested
; substitution1 and substitution2 are each a pairlist:  (A1 B1) and (A2 B2)
;output-spec
; if an item in L matches A1  then the result has B1
; substituted for in the rest. Failing that if the item matches A2 then it is
; Substituted by B2.  Otherwise it is left unchanged.
;  Note that the result list is a whole new structure with values copied
; over from L, the original list is not modified.

; [6 pts]
(define (parallelSubstitute2 L substitution1 substitution2)
   (if (null? L)
       '()
       (if (equal? (first L) (first substitution1))
           (cons (second substitution1) (parallelSubstitute2 (rest L) substitution1 substitution2))
           (if (equal? (first L) (first substitution2))
               (cons (second substitution2) (parallelSubstitute2 (rest L) substitution1 substitution2))
               (cons (first L) (parallelSubstitute2 (rest L) substitution1 substitution2))
           )
       )
   )
)


(module+ test
  (check-equal? (parallelSubstitute2 '( 1 3 5) '(3 5) '(5 7)) '(1 5 7))
  (check-equal? (parallelSubstitute2 '(a g f) '(a b) '(g c)) '(b c f))
  (check-equal? (parallelSubstitute2 '( 1 f 5) '(1 w) '(f 4)) '(w 4 5))

  )

  ;input-spec item is a value, possibly a list.  substitution is a list of pairs ( (A1 B1) (A2 B2) ...)
; output-spec if item matches Ai (and no Aj for j<i then B2 is returned.

; [6 pts]
(define (parallelSubsOfItem item substitutions)
    (if (null? substitutions)
        item
        (if (equal? item (first (first substitutions)))
            (second (first substitutions))
            (parallelSubsOfItem item (rest substitutions))
        )
    )
)

(module+ test
  (check-equal? (parallelSubsOfItem 3 '( (1 3) (3 5))) 5)
  (check-equal? (parallelSubsOfItem '(1 3) '( ( (1 3) 5) (5 7))) 5)
  (check-equal? (parallelSubsOfItem '() '( ( (1 3) 5) (5 7))) '())
  (check-equal? (parallelSubsOfItem '(1 3) '()) '(1 3))
  (check-equal? (parallelSubsOfItem '() '()) '(1 3))
                ; need more tests!
  )
                      
;input-spec L is a list possibly nested
; substitution is a list of pairs  ( ( A1 B1) (A2 B2)...)
;output-spec
; if an item in L matches Ai (and no Aj for j<i) then that item is Substituted
; by Bi in the rest
  ;This one is a freebie to show you that the code for these functions is usually short and sweet,
  ; once you have the right pieces/helpers.
(define (parallelSubstituteMulti L substitutions)
  (if (null? L) L ; if L is empty result is the empty list
      (cons (parallelSubsOfItem (first L) substitutions)
            (parallelSubstituteMulti (rest L) substitutions))))

(module+ test
 (check-equal? (parallelSubstituteMulti '( 1 3 5) '((3 5) (5 7))) '(1 5 7))
 (check-equal? (parallelSubstituteMulti '() '((3 5) (5 7))) '())
 (check-equal? (parallelSubstituteMulti '( 1 3 5) '()) '(1 3 5))
)

; input-spec  L is an arbitrary list or possibly an atom.
; substitution is a list of substitution pairs, each item a  list of the form ( a b )
; where a and b can be arbitrary Racket expressions, either a list or atom.

; output-spec result is L with the substitution applied.  The match
; will be searched for sublists of L as well as the top-level of L.

; [6 pts]
(define (parallelSubstituteRMulti L substitutions)
  (if (null? L)
      L;
      (if (not (equal? (parallelSubsOfItem (first L) substitutions) (first L))) 
          (cons (parallelSubsOfItem (first L) substitutions)
                (parallelSubstituteRMulti (rest L) substitutions))
          (if (list? (first L))
              (append (cons
                          (parallelSubstituteRMulti (first L) substitutions)
                          '())
                      (parallelSubstituteRMulti (rest L) substitutions)
              )
              (cons (first L) (parallelSubstituteRMulti (rest L) substitutions))
          )
      )
   )
)
  
(module+ test
 (check-equal? (parallelSubstituteRMulti '( 1 3 5) '((3 5) (5 7))) '(1 5 7))
 (check-equal? (parallelSubstituteRMulti '() '((3 5) (5 7))) '())
  (check-equal? (parallelSubstituteRMulti '( 1 3 5) '()) '(1 3 5))
  (check-equal? (parallelSubstituteRMulti '( 1 (3 5) 5 (6) ) '( (5 6) (6 47))) '(1 (3 6) 6 (47)))
  (check-equal? (parallelSubstituteRMulti '(1 (1 5) (1 5 (3 (1 5)  (1 (1 5) 3) (1 5) )) 3) '( ( (1 5) 6 ) (6 47) ( ( 1 6 3) (a (b) c) ))) '(1 6 (1 5 (3 6 (1 6 3) 6)) 3))
  (check-equal? (parallelSubstituteRMulti '( 1 (3 5) 5 (6) ) '( )) '(1 (3 5) 5 (6)))
  (check-equal? (parallelSubstituteRMulti '(  ) '( (1 3) )) '() )
)