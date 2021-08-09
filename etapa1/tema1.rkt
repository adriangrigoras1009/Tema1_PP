#lang racket/gui


(define-struct counter (index tt queue))
(define C1 (make-counter 1 25 '((ion . 15)(matei . 10))))
(define C2 (make-counter 1 20 '((jon . 11)(matheei . 9))))

(define (empty-counter index)
  (define C (make-counter index 0 '()))
  C)

(define (add-to-counter C name n-items)
  (define x (cons name n-items))
  (define y (list x))
  (define C_nou (struct-copy counter C [queue (append (counter-queue C) y)]))
  C_nou)

;(add-to-counter C1 "jona" 10)
(counter-queue (add-to-counter C1 'jona 10))
  
(define (tt+ C minutes)
  (define x (+ (match C [(counter index tt queue) tt]) minutes))
  (define C_nou (struct-copy counter C [tt x]))
  C_nou)

(counter-queue (tt+ C1 10))

(define (recursive counters)
  (cond
    ((null? (cdr counters)) (car counters))
    ((< (counter-tt (car counters)) (counter-tt (recursive (cdr counters)))) (car counters))
    (else (recursive (cdr counters)))))

(define (min-tt counters)
  (define C (recursive counters))
  (define pereche (cons (counter-index C) (counter-tt C)))
  pereche)

(min-tt (list C1 C2))


(define (serve requests C1 C2 C3 C4)
  'your_code)