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

(define gt
  (lambda (n m)
    (cond
     ((zero? n) #f)
     ((zero? m) #t)
     (else
      (gt (sub1 n) (sub1 m))))))

(define lt
  (lambda (n m)
    (cond
     ((zero? m) #f)
     ((zero? n) #t)
     (else
      (lt (sub1 n) (sub1 m))))))

(define equal
  (lambda (n m)
    (cond
     ((gt n m) #f)
     ((lt n m) #f)
     (else #t))))

(define equal?
  (lambda (s1 s2)
    (cond
     ((and (atom? s1) (atom? s2)) (eqan? s1 s2))
     ((or (atom? s1) (atom? s2)) #f)
     (else
      (eqlist? s1 s2)))))

(define eqan?
  (lambda (n m)
    (cond
     ((and (number? n) (number? m)) (equal n m))
     ((number? n) #f)
     (else (eq? n m)))))

(define eqlist?
  (lambda (l1 l2)
    (cond
     ((and (null? l1) (null? l2)) #t)
     ((or (null? l1) (null? l2)) #f)
     (else
      (and (equal? (car l1) (car l2))
	   (equal? (cdr l1) (cdr l2)))))))

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

(define 1st-sub-exp
  (lambda (aexp)
    (car aexp)))

(define operator
  (lambda (aexp)
    (car (cdr aexp))))

(define 2nd-sub-exp
  (lambda (aexp)
    (car (cdr (cdr aexp)))))

;; Off we go! First version of `rember-f`

(define rember1-f
  (lambda (test? a l)
    (cond
     ((null? l) '())
     ((test? a (car l)) (cdr l))
     (else
      (cons (car l) (rember1-f test? a (cdr l)))))))

;; The first `*-c` functions---for `compare`, perhaps? (or for Curry?)

(define eq?-c
  (lambda (a)
    (lambda (x)
      (eq? a x))))

;; Second version of `rember-f`---tricky! When recursing, we have no name to
;; recurse ... But we don't need to, that was the point of all those questions
;; about whether we needed the name of the function X to call it!

(define rember2-f
  (lambda (test?)
    (lambda (a l)
      (cond
       ((null? l) '())
       ((test? a (car l)) (cdr l))
       (else
	(cons (car l)
	      ((rember2-f test?) a (cdr l))))))))

;; The left and right inserts now:

(define insertL-f
  (lambda (test?)
    (lambda (new old l)
      (cond
       ((null? l) '())
       ((test? (car l) old)
	(cons new (cons old (cdr l))))
       (else
	(cons (car l)
	      ((insertL-f test?) new old (cdr l))))))))

(define insertR-f
  (lambda (test?)
    (lambda (new old l)
      (cond
       ((null? l) '())
       ((test? (car l) old)
	(cons old (cons new (cdr l))))
       (else
	(cons (car l)
	      ((insertR-f test?) new old (cdr l))))))))

;; My attempt at `insert-g`. Currying also on trhe `test?`, unlike in the book.

(define insert-left
  (lambda (new l)
    (cons new l)))

(define insert-right
  (lambda (new l)
    (cons (car l) (cons new (cdr l)))))

(define insert-g-mine
  (lambda (insert-f)
    (lambda (test?)
      (lambda (new old l)
	(cond
	 ((null? l) '())
	 ((test? (car l) old)
	  (insert-f new l))
	 (else
	  (cons (car l)
		(((insert-g-mine insert-f) test?) new old (cdr l)))))))))

;; The version given in the text is different, based on `seqL` and `seqR`.
;; The technique used in the book replaces the differing code with functions
;; whose arguments mimic the variable names used in the code which is being
;; replaced:

(define seqL
  (lambda (new old l)
    (cons new (cons old l))))

(define seqR
  (lambda (new old l)
    (cons old (cons new l))))

(define insert-g
  (lambda (seq)
    (lambda (new old l)
      (cond
       ((null? l) '())
       ((eq? (car l) old)
	(seq new old (cdr l)))
       (else
	(cons (car l)
	      ((insert-g seq) new old (cdr l))))))))

;; On our way now to replace `value` from chapter 6.

(define atom-to-function
  (lambda (x)
    (cond
     ((eq? x 'plus) plus)
     ((eq? x 'times) times)
     (else raise))))

(define value
  (lambda (nexp)
    (cond
     ((atom? nexp) nexp)
     (else
      ((atom-to-function (operator nexp))
       (value (1st-sub-exp nexp))
       (value (2nd-sub-exp nexp)))))))

;; `multirember-f`

(define multirember-f
  (lambda (test?)
    (lambda (a lat)
      (cond
       ((null? lat) '())
       ((test? a (car lat))
	((multirember-f test?) a (cdr lat)))
       (else
	(cons (car lat)
	      ((multirember-f test?) a (cdr lat))))))))

;; Now we start building our way to _The Tenth Commandment_:
;;
;; > Build functions to collect one more value at a time.
;;
;; A _collector_ (also called a _continuation_ sometimes, according to the
;; text) is a function that collects values through its arguments.
;;
;; The first example is `multirember&co`. Its return value will be the
;; evaluation of the collector funtion at the terminal case.

(define multirember&co
  (lambda (a lat col)
    (cond
     ((null? lat)
      (col '() '()))
     ((eq? (car lat) a)
      (multirember&co a
		      (cdr lat)
		      (lambda (newlat seen)
			(col newlat
			     (cons (car lat) seen)))))
     (else
      (multirember&co a
		      (cdr lat)
		      (lambda (newlat seen)
			(col (cons (car lat) newlat)
			     seen)))))))

;; Now we define `multiinsertLR`, which does the job of `multiinsertL` and
;; `multiinsertR` in the same pass:

(define multiinsertLR
  (lambda (new oldL oldR lat)
    (cond
     ((null? lat) '())
     ((eq? (car lat) oldL)
      (cons new
	    (cons new
		  (multiinsertLR new oldL oldR (cdr lat)))))
     ((eq? (car lat) oldR)
      (cons (car lat)
	    (cons new
		  (multiinsertLR new oldL oldR (cdr lat)))))
     (else
      (cons (car lat)
	    (multiinsertLR new oldL oldR (cdr lat)))))))

;; Based on this, we will build `multiinsertLR&co`, which will also keep track
;; of the number of insertions to the left and to the right.
;;
;; What we want with this new functions is:
;;
;; 1. The new list with the insertions done at the right place.
;; 2. The number of left insertions, and
;; 3. The number of the right insertions.
;;
;; That means tha our collector will have the following arguments:
;;
;;     (newlat L R)
;;
;; with `newlat` being a list, and both `L` and `R` integers.

(define multiinsertLR&co
  (lambda (new oldL oldR lat col)
    (cond
     ((null? lat)
      (col '() 0 0))
     ((eq? (car lat) oldL)
      (multiinsertLR&co new
			oldL
			oldR
			(cdr lat)
			(lambda (newlat L R)
			  (col (cons new (cons oldL newlat))
			       (add1 L)
			       R))))
     ((eq? (car lat) oldR)
      (multiinsertLR&co new
			oldL
			oldR
			(cdr lat)
			(lambda (newlat L R)
			  (col (cons oldR (cons new newlat))
			       L
			       (add1 R)))))
     (else
      (multiinsertLR&co new
			oldL
			oldR
			(cdr lat)
			(lambda (newlat L R)
			  (col (cons (car lat) newlat)
			       L
			       R)))))))

;; In other words: We collect multiple values by building functions which
;; capture those values in their argument lists.
;;
;; That's why we *build* a new collector function on each pass.
;;
;; That's also why, in this particular case, we build up `newlat` and
;; potentially increase `L` and `R` on each pass

;; Back to star-functions from chapter 5. We build first `evens-only*`, a
;; function to remove all odd numbers from a sexp.

(define evens-only*
  (lambda (l)
    (cond
     ((null? l) '())
     ((atom? (car l))
      (cond
       ((even? (car l))
	(cons (car l) (evens-only* (cdr l))))
       (else
	(evens-only* (cdr l)))))
     (else
      (cons (evens-only* (car l))
	    (evens-only* (cdr l)))))))

;; Now we build `evens-only*&co`, which will collect:
;;
;; 1. The list of all even numbers, and
;; 2. The product of all even numbers, and
;; 3. The sum of all odd numbers.

(define evens-only*&co
  (lambda (l col)
    (cond
     ((null? l) (col '() 1 0))
     ((atom? (car l))
      (cond
       ((even? (car l))
	(evens-only*&co (cdr l)
			(lambda (newl product sum)
			  (col (cons (car l) newl)
			       (* product (car l))
			       sum))))
       (else
	(evens-only*&co (cdr l)
			(lambda (newl product sum)
			  (col newl
			       product
			       (+ sum (car l))))))))
     (else
      (evens-only*&co (car l)
		      (lambda (newl product sum)
			(evens-only*&co (cdr l)
					(lambda (newl2 product2 sum2)
					  (col (cons newl newl2)
					       (* product product2)
					       (+ sum sum2))))))))))
