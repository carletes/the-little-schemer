(define (atom? x)
  (and (not (pair? x)) (not (null? x))))

(define add1
  (lambda (n)
    (+ n 1)))

(define sub1
  (lambda (n)
    (- n 1)))

;; My first version of `+`. I swapped the usage of `add1` and `+`, and I named
;; the arguments in reverse order

(define my-plus
  (lambda (m n)
    (cond
     ((zero? n) m)
     (else
      (my-plus (add1 m) (sub1 n))))))

(define plus
  (lambda (n m)
    (cond
     ((zero? m) n)
     (else
      (add1 (plus n (sub1 m)))))))

;; My first version of minus, inspired by `plus`.

(define minus
  (lambda (n m)
    (cond
     ((zero? m) n)
     (else
      (sub1 (minus n (sub1 m)))))))

;; My first version of `addtup`

(define addtup
  (lambda (tup)
    (cond
     ((null? tup) 0)
     (else
      (plus (car tup) (addtup (cdr tup)))))))

(define times
  (lambda (n m)
    (cond
     ((zero? m) 0)
     (else
      (plus n (times n (sub1 m)))))))

;; My first version of `tup+`, matching the proposed solution in the book

(define my-first-tup+
  (lambda (tup1 tup2)
    (cond
     ((and (null? tup1) (null? tup2)) '())
     (else
      (cons (plus (car tup1) (car tup2))
	    (my-first-tup+ (cdr tup1) (cdr tup2)))))))

;; My version of `tup+` that also works for tuples of different length. I did
;; not spot the simplifaction.

(define my-next-tup+
  (lambda (tup1 tup2)
    (cond
     ((and (null? tup1) (null? tup2)) '())
     ((null? tup1) tup2)
     ((null? tup2) tup1)
     (else
      (cons (plus (car tup1) (car tup2))
	    (my-next-tup+ (cdr tup1) (cdr tup2)))))))

;; The simplified version of `tup+`

(define tup+
  (lambda (tup1 tup2)
    (cond
     ((null? tup1) tup2)
     ((null? tup2) tup1)
     (else
      (cons (plus (car tup1) (car tup2))
	    (tup+ (cdr tup1) (cdr tup2)))))))

;; My initial `>`. The order of the two terminal conditions, unlike in `tup+`
;; above, matters.

(define gt
  (lambda (n m)
    (cond
     ((zero? n) #f)
     ((zero? m) #t)
     (else
      (gt (sub1 n) (sub1 m))))))

;; My not-so-initial `<`. Like in `>` above, the order of the two terminal
;; conditions matters (and is reverse)
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

(define raise
  (lambda (n m)
    (cond
     ((zero? m) 1)
     (else
      (times n (raise n (sub1 m)))))))

;; Division. Which means, according to TLS, "how many times `m` fits into `n`.
;; That definitions translates neatly into the implementation!
;;
;; Note that here we're recursing into the *first* argument
(define div
  (lambda (n m)
    (cond
     ((lt n m) 0)
     (else
      (add1 (div (minus n m) m))))))

(define length
  (lambda (lat)
    (cond
     ((null? lat) 0)
     (else
      (add1 (length (cdr lat)))))))

;; My initial `pick`. Less efficient than the one proposed, since `equal` is
;; expensive!
(define my-pick
  (lambda (n lat)
    (cond
     ((equal n 1) (car lat))
     (else
      (my-pick (sub1 n) (cdr lat))))))

(define pick
  (lambda (n lat)
    (cond
     ((zero? (sub1 n)) (car lat))
     (else
      (pick (sub1 n) (cdr lat))))))

(define rempick
  (lambda (n lat)
    (cond
     ((zero? (sub1 n)) (cdr lat))
      (else
       (cons (car lat) (rempick (sub1 n) (cdr lat)))))))

(define no-nums
  (lambda (lat)
    (cond
     ((null? lat) '())
     (else
      (cond
       ((number? (car lat)) (no-nums (cdr lat)))
       (else
	(cons (car lat) (no-nums (cdr lat)))))))))

(define all-nums
  (lambda (lat)
    (cond
     ((null? lat) '())
     (else
      (cond
       ((number? (car lat)) (cons (car lat) (all-nums (cdr lat))))
       (else
	(all-nums (cdr lat))))))))

;; My version of `eqan?`. Simpler that the one from TLS: It delegates the third
;; question (`(number? m)`) to the `else` clause.
(define eqan?
  (lambda (n m)
    (cond
     ((and (number? n) (number? m)) (equal n m))
     ((number? n) #f)
     (else (eq? n m)))))

(define occur
  (lambda (a lat)
    (cond
     ((null? lat) 0)
     (else
      (cond
       ((eq? a (car lat)) (add1 (occur a (cdr lat))))
       (else (occur a (cdr lat))))))))
