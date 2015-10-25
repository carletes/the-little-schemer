;; From previous chapters

(define (atom? x)
  (and (not (pair? x)) (not (null? x))))

(define add1
  (lambda (n)
    (+ n 1)))

(define sub1
  (lambda (n)
    (- n 1)))

(define plus
  (lambda (n m)
    (cond
     ((zero? m) n)
     (else
      (add1 (plus n (sub1 m)))))))

(define minus
  (lambda (n m)
    (cond
     ((zero? m) n)
     (else
      (sub1 (minus n (sub1 m)))))))

(define times
  (lambda (n m)
    (cond
     ((zero? m) 0)
     (else
      (plus n (times n (sub1 m)))))))

(define raise
  (lambda (n m)
    (cond
     ((zero? m) 1)
     (else
      (times n (raise n (sub1 m)))))))

;; Off we go! Looks like we're starting to build an evaluator for arithmetic
;; expressions.
;;
;; An _arithmetic expression_ is either:
;;
;;   * An atom, or
;;   * Two artihmetic expressions combined by the atoms `plus`, `times` or
;;     `raise`
;;
;; A _representation_ for an arithmetic expression is a sexp whose content is
;; an arithmetic expression


;; `numbered?`: A function that returns whether a given sexp is a
;; representation of an arithmetic expression whose atoms are either numbers,
;; or the atoms `plus`, `times` or `raise.

;; My first attempt, which does *not* assume its input is a representation of
;; an arithmetic expression, just a sexp (I overlooked the name of the argument)
(define my-numbered?
  (lambda (aexp)
    (cond
     ((atom? aexp) (number? aexp))
     ((eq? (car (cdr aexp)) 'plus)
      (and (my-numbered? (car aexp)) (my-numbered? (car (cdr (cdr aexp))))))
     ((eq? (car (cdr aexp)) 'times)
      (and (my-numbered? (car aexp)) (my-numbered? (car (cdr (cdr aexp))))))
     ((eq? (car (cdr aexp)) 'raise)
      (and (my-numbered? (car aexp)) (my-numbered? (car (cdr (cdr aexp))))))
     (else #f))))

;; The version given in TLS, which assumes the input is an arithmetic
;; expression

(define numbered?
  (lambda (aexp)
    (cond
     ((atom? aexp) (number? aexp))
     (else
      (and (numbered? (car aexp))
	   (numbered? (car (cdr (cdr aexp)))))))))

;; `value`: A function that returns the numerical value of the representation
;; of an arithmetic expression

(define value0
  (lambda (nexp)
    (cond
     ((atom? nexp) nexp)
     ((eq? (car (cdr nexp)) 'plus)
      (plus (value0 (car nexp)) (value0 (car (cdr (cdr nexp))))))
     ((eq? (car (cdr nexp)) 'times)
      (times (value0 (car nexp)) (value0 (car (cdr (cdr nexp))))))
     (else
      (raise (value0 (car nexp)) (value0 (car (cdr (cdr nexp)))))))))

;; Now with helper functions, much cleaner

(define 1st-sub-exp
  (lambda (aexp)
    (car aexp)))

(define operator
  (lambda (aexp)
    (car (cdr aexp))))

(define 2nd-sub-exp
  (lambda (aexp)
    (car (cdr (cdr aexp)))))

(define value
  (lambda (nexp)
    (cond
     ((atom? nexp) nexp)
     ((eq? (operator nexp) 'plus)
      (plus (value0 (1st-sub-exp nexp)) (value0 (2nd-sub-exp nexp))))
     ((eq? (operator nexp) 'times)
      (times (value0 (1st-sub-exp nexp)) (value0 (2nd-sub-exp nexp))))
     (else
      (raise (value0 (1st-sub-exp nexp)) (value0 (2nd-sub-exp nexp)))))))
