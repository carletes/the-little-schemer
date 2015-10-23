;; First attempt at `rember`. Calling `(car lat)` twice, sice we have not
;; defined `let` yet.
;;
;; I jumped straight ahead into the simplified version

(define rember
  (lambda (a lat)
    (cond
     ((null? lat) '())
     ((eq? a (car lat)) (cdr lat))
     (else (cons (car lat) (rember a (cdr lat)))))))

(define firsts
  (lambda (l)
    (cond
     ((null? l) '())
     (else (cons (car (car l)) (firsts (cdr l)))))))

(define insertR
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     (else
      (cond
       ((eq? (car lat) old)
	(cons old
	      (cons new (cdr lat))))
       (else (cons (car lat)
		   (insertR new old (cdr lat)))))))))

;; My initial version pf `insertL`: Just swap the insertion order of `insertR`
(define my-insertL
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     (else
      (cond
       ((eq? (car lat) old)
	(cons new
	      (cons old (cdr lat))))
       (else (cons (car lat)
		   (my-insertL new old (cdr lat)))))))))

;; ... overlooking the fact that `car lat` is `old`!

(define insertL
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     (else
      (cond
       ((eq? (car lat) old)
	(cons new lat))
       (else (cons (car lat)
		   (insertL new old (cdr lat)))))))))

(define subst
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     (else
      (cond
       ((eq? (car lat) old)
	(cons new (cdr lat)))
       (else (cons (car lat)
		   (subst new old (cdr lat)))))))))
