;; From previous chapters

(define sub1
  (lambda (n)
    (- n 1)))

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

(define member?
  (lambda (a lat)
    (cond
     ((null? lat) #f)
     (else (or (eq? a (car lat))
	       (member? a (cdr lat)))))))

(define multirember
  (lambda (a lat)
    (cond
     ((null? lat) '())
     ((eq? a (car lat)) (multirember a (cdr lat)))
     (else (cons (car lat) (multirember a (cdr lat)))))))

;; Off we go!

(define set?
  (lambda (lat)
    (cond
     ((null? lat) #t)
     ((member? (car lat) (cdr lat)) #f)
     (else (set? (cdr lat))))))

;; First `makeset`: Using `member?`

(define makeset1
  (lambda (lat)
    (cond
     ((null? lat) '())
     ((member? (car lat) (cdr lat)) (makeset1 (cdr lat)))
     (else
      (cons (car lat) (makeset1 (cdr lat)))))))

;; `makeset` with `multirember`

(define makeset2
  (lambda (lat)
    (cond
     ((null? lat) '())
     (else
      (cons (car lat)
	    (multirember (car lat) (makeset2 (cdr lat))))))))

(define subset?
  (lambda (set1 set2)
    (cond
     ((null? set1) #t)
     (else
      (and (member? (car set1) set2)
	   (subset? (cdr set1) set2))))))

(define eqset?
  (lambda (set1 set2)
    (and (subset? set1 set2)
	 (subset? set2 set1))))

(define intersect?
  (lambda (set1 set2)
    (cond
     ((null? set1) #f)
     (else
      (or (member? (car set1) (set2))
	  (intersect? (cdr set1) set2))))))

;;`intersect?` is like `subset?`

(define intersect
  (lambda (set1 set2)
    (cond
     ((null? set1) '())
     ((member (car set1) set2)
      (cons (car set1) (intersect (cdr set1) set2)))
     (else
      (intersect (cdr set1) set2)))))

(define union
  (lambda (set1 set2)
    (cond
     ((null? set1) set2)
     ((member? (car set1) set2)
      (union (cdr set1) set2))
     (else
      (cons (car set1) (union (cdr set1) set2))))))

(define intersect-all
  (lambda (l-set)
    (cond
     ((null? (cdr l-set)) (car l-set))
     (else
      (intersect (car l-set) (intersect-all (cdr l-set)))))))
