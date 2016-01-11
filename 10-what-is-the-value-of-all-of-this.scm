;; From previous chapters:

(define (atom? x)
  (and (not (pair? x)) (not (null? x))))

(define first
  (lambda (pair)
    (car pair)))

(define second
  (lambda (pair)
    (car (cdr pair))))

(define add1
  (lambda (n)
    (+ n 1)))

(define sub1
  (lambda (n)
    (- n 1)))

;; Off we go!
;;
;; Looks like the aim of this chapter is to build an (limited) expression
;; evaluator.

;; `new-entry`

(define new-entry
  (lambda (names values)
    (cons names (cons values '()))))

;; My first attempt at building `lookup-in-entry`:

(define lookup-in-entry0
  (lambda (name entry not-found)
    (cond
     ((null? (car entry)) (not-found name))
     ((eq? name (car (car entry))) (car (car (cdr entry))))
     (else
      (lookup-in-entry0 name (new-entry (cdr (car entry)) (cdr (car (cdr entry)))) not-found)))))

;; The text recalls `first` and `second` from previous chapters, and introduces
;; a helper function:

(define lookup-in-entry
  (lambda (name entry entry-f)
    (lookup-in-entry-help name
			  (first entry)
			  (second entry)
			  entry-f)))

(define lookup-in-entry-help
  (lambda (name names values entry-f)
    (cond
     ((null? names) (entry-f name))
     ((eq? (car names) name) (car values))
     (else (lookup-in-entry-help name
				 (cdr names)
				 (cdr values)
				 entry-f)))))

;; A *table* (or *environment*) is defined as a list of entries. My intuitive
;; definition for *table* would be that of *entry*, but thinking about Scheme's
;; lexical scope, the book's definition makes sense.

;; `lookup-in-table`

(define lookup-in-table
  (lambda (name table table-f)
    (cond
     ((null? table) (table-f name))
     (else
      (lookup-in-entry name
		       (car table)
		       (lambda (name)
			 (lookup-in-table name (cdr table) table-f)))))))

;; First steps towards our own evaluator

(define expression-to-action
  (lambda (e)
    (cond
     ((atom? e) (atom-to-action e))
     (else
      (list-to-action e)))))

(define atom-to-action
  (lambda (e)
    (cond
     ((number? e) *const)
     ((eq? e #t) *const)
     ((eq? e #f) *const)
     ((eq? e (quote cons)) *const)
     ((eq? e (quote car))  *const)
     ((eq? e (quote cdr)) *const)
     ((eq? e (quote null?)) *const)
     ((eq? e (quote eq?)) *const)
     ((eq? e (quote atom?)) *const)
     ((eq? e (quote zero?)) *const)
     ((eq? e (quote add1)) *const)
     ((eq? e (quote sub1)) *const)
     ((eq? e (quote number?)) *const)
     (else *identifier))))

(define list-to-action
  (lambda (e)
    (cond
     ((atom? (car e))
      (cond
       ((eq? (car e) (quote quote)) *quote)
       ((eq? (car e) (quote lambda)) *lambda)
       ((eq? (car e) (quote cond)) *cond)
       (else *application)))
     (else *application))))

;; The function `value` is our interpreter

(define value
  (lambda (e)
    (meaning e (quote ()))))

(define meaning
  (lambda (e table)
    ((expression-to-action e) e table)))

;; The actions come now:

(define *const
  (lambda (e table)
    (cond
     ((number? e) e)
     ((eq? e #t) #t)
     ((eq? e #f) #f)
     ((else
       (build (quote primitive) e))))))

(define *quote
  (lambda (e table)
    (second e)))

(define *identifier
  (lambda (e table)
    (lookup-in-table e table initial-table)))

(define initial-table
  (lambda (name)
    (car (quote ()))))
