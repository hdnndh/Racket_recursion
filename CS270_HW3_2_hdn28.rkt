#lang racket

(define  (addRock mRock nRock)
    (append mRock nRock)
)

 (eq?  (addRock ’(rock rock) ’(rock) ) ’(rock rock rock ))  ;To be proven
≡ (eq?  (append ’(rock rock) ’(rock) ) ’(rock rock rock ))  ;Definition of addRock
≡ (eq?  ’(rock rock rock)    ’(rock rock rock ))            ;Evaluate append
≡ True  ;Prove finished