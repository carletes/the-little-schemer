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

;; Let's go!

(define rember*
  (lambda (a lat)
    (cond
     ((null? lat) '())
     (else
      (cond
       ((atom? (car lat))
	(cond
	 ((eq? (car lat) a) (rember* a (cdr lat)))
	 (else (cons (car lat) (rember* a (cdr lat))))))
       (else
	(cons (rember* a (car lat)) (rember* a (cdr lat)))))))))

(define lat?
  (lambda (l)
    (cond
     ((null? l) #t)
     (else
      (cond
       ((atom? (car l)) (lat? (cdr l)))
       (else #f))))))

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

(define eqan?
  (lambda (n m)
    (cond
     ((and (number? n) (number? m)) (equal n m))
     ((number? n) #f)
     (else (eq? n m)))))

;; My version of `insertR*`. Cheating a bit: I read the final version of the
;; First Commandment first, so I ask three questions straight away.

(define insertR*
  (lambda (new old l)
    (cond
     ((null? l) '())
     ((atom? (car l))
      (cond
       ((eq? (car l) old)
	(cons (car l) (cons new (insertR* new old (cdr l)))))
       (else
	(cons (car l) (insertR* new old (cdr l))))))
     (else
      (cons (insertR* new old (car l)) (insertR* new old (cdr l)))))))

(define occur*
  (lambda (a l)
    (cond
     ((null? l) 0)
     ((atom? (car l))
      (cond
       ((eq? (car l) a) (add1 (occur* a (cdr l))))
       (else
	(occur* a (cdr l)))))
     (else
      (plus (occur* a (car l)) (occur* a (cdr l)))))))

(define subst*
  (lambda (new old l)
    (cond
     ((null? l) '())
     ((atom? (car l))
      (cond
       ((eq? (car l) old)
	(cons new (subst* new old (cdr l))))
       (else
	(cons (car l) (subst* new old (cdr l))))))
     (else
      (cons (subst* new old (car l)) (subst* new old (cdr l)))))))

(define insertL*
  (lambda (new old l)
    (cond
     ((null? l) '())
     ((atom? (car l))
      (cond
       ((eq? (car l) old)
	(cons new (cons old (insertL* new old (cdr l)))))
       (else
	(cons (car l) (insertL* new old (cdr l))))))
     (else
      (cons (insertL* new old (car l)) (insertL* new old (cdr l)))))))

(define member*
  (lambda (a l)
    (cond
     ((null? l) #f)
     ((atom? (car l))
      (cond
       ((eq? (car l) a) #t)
       (else
	(member* a (cdr l)))))
     (else
      (or (member* a (car l)) (member* a (cdr l)))))))

(define leftmost
  (lambda (l)
    (cond
     ((atom? (car l)) (car l))
     (else
      (leftmost (car l))))))

;; My first take on `eqlist?`. Simpler than the first version given, since I
;; jumped straight away into the shortcuts in the first terminal clauses.

(define my-eqlist?
  (lambda (l1 l2)
    (cond
     ((null? l1) (null? l2))
     ((null? l2) #f)
     ((atom? (car l1))
      (cond
       ((atom? (car l2))
	(and (eqan? (car l1) (car l2))
	     (my-eqlist? (cdr l1) (cdr l2))))
       (else #f)))
     ((atom? (car l2)) #f)
     (else
      (and (my-eqlist? (car l1) (car l2))
	   (my-eqlist? (cdr l1) (cdr l2)))))))

;; The second `eqlist?` fron LTS. Simpler than mine: Fewer terminal clauses
;; for the `atom? (car ..)` cases

(define eqlist2?
  (lambda (l1 l2)
    (cond
     ((and (null? l1) (null? l2)) #t)
     ((or (null? l1) (null? l2)) #f)
     ((and (atom? (car l1)) (atom? (car l2)))
      (and (eqan? (car l1) (car l2))
	   (eqlist2? (cdr l1) (cdr l2))))
     ((or (atom? l1) (atom? l2)) #f)
     (else
     (and (eqlist2? (car l1) (car l2))
	  (eqlist2? (cdr l1) (cdr l2)))))))

;; My first version of `equal?`. Uses the fact that `eqan?` behaves fine when
;; passed a list.

(define my-equal?
  (lambda (s1 s2)
    (cond
     ((atom? s1) (eqan? s1 s2))
     ((atom? s2) #f)
     (else
      (eqlist? s1 s2)))))

;; The version of `equal?` given

(define equal?
  (lambda (s1 s2)
    (cond
     ((and (atom? s1) (atom? s2)) (eqan? s1 s2))
     ((or (atom? s1) (atom? s2)) #f)
     (else
      (eqlist? s1 s2)))))

;; ... and a rewrite of `eqlist?` using it, so that we end up with one defined
;; in terms of the other.

(define eqlist?
  (lambda (l1 l2)
    (cond
     ((and (null? l1) (null? l2)) #t)
     ((or (null? l1) (null? l2)) #f)
     (else
      (and (equal? (car l1) (car l2))
	   (equal? (cdr l1) (cdr l2)))))))

;; `rember` for lists of sexps, first version

(define rember1
  (lambda (s l)
    (cond
     ((null? l) '())
     ((atom? (car l))
      (cond
       ((equal? (car l) s) (cdr l))
       (else (rember1 s (cdr l)))))
     (else
      (cond
       ((equal? (car l) s) (cdr l))
       (else
	(cons (car l) (rember1 s (cdr l)))))))))

;; Second version: `equal?` works on all types of sexps, so:

(define rember2
  (lambda (s l)
    (cond
     ((null? l) '())
     ((equal? s (car l)) (cdr l))
     (else (cons (car l) (rember2 s (cdr l)))))))
