(define looking
  (lambda (a lat)
    (keep-looking a (pick 1 lat) lat)))

;; My first attempt at `keep-looking`

(define keep-looking-mine
  (lambda (a candidate lat)
    (cond
     ((eq? a candidate) #t)
     ((number? candidate) (keep-looking-mine a (pick candidate lat) lat))
     (else #f))))

(define pick
  (lambda (pos lat)
    (cond
     ((eq? pos 1) (car lat))
     (else
      (pick (- pos 1) (cdr lat))))))

;; The version offered in the book uses `sorn` ("symbol-or-number") as its
;; second parameter name, and the clauses are rearranged (and simpler):
;;
;; The idea of _unnatural recursion_ is introduced here, since we're not
;; recurring on a part of `lat`.

(define keep-looking
  (lambda (a sorn lat)
    (cond
     ((number? sorn)
      (keep-looking a (pick sorn lat) lat))
     (else
      (eq? a sorn)))))

;; Functions like `keep-looking` are called _partial functions_, because they
;; may not terminate (unlike all other functions we've seen so far, which are
;; called _total functions_.
