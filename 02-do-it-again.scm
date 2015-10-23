(define (atom? x)
  (and (not (pair? x)) (not (null? x))))

;;; My version of `lat?` from what I remembered from Scheme:
(define (my-lat? lst)
  (if (null? lst)
      #t
      (and (atom? (car lst)) (lat? (cdr lst)))))

;;; The version presented in the book.
(define lat?
  (lambda (l)
    (cond
     ((null? l) #t)
     ((atom? (car l)) (lat? (cdr l)))
     (else #f))))

(define member?
  (lambda (a lat)
    (cond
     ((null? lat) #f)
     (else (or (eq? a (car lat))
	       (member? a (cdr lat)))))))
