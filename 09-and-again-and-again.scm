;; From previous chapters

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

(define times
  (lambda (n m)
    (cond
     ((zero? m) 0)
     (else
      (plus n (times n (sub1 m)))))))

(define (atom? x)
  (and (not (pair? x)) (not (null? x))))

(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 '()))))

(define first
  (lambda (p)
    (car p)))

(define second
  (lambda (p)
    (car (cdr p))))

(define a-pair?
  (lambda (x)
    (cond
     ((atom? x) #f)
     ((null? x) #f)
     ((null? (cdr x)) #f)
     ((null? (cdr (cdr x))) #t)
     (else #f))))

;; Off we go!

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

(define shift
  (lambda (pair)
    (build
     (first (first pair))
     (build
      (second (first pair))
      (second pair)))))

;; `align`, defined here, is potentially partial:

(define align
  (lambda (pora)
    (cond
     ((atom? pora) pora)
     ((a-pair? (first pora))
      (align (shift pora)))
     (else
      (build (first pora)
	     (align (second pora)))))))

;; According to the text, it is potentially partial because it violates the
;; Seventh Commandment: `(align (shift pora))` creates an argument for
;; the recursive call to `align` which is not part of the original argument.
;;
;; However, if we could show that the arguments to the recursove calls of
;; `align` get *simpler* (that is, closer to being an atom), then we could be
;; certin that `align` is total.
;;
;; The text now goes on to build ways of quantifying how much simpler `pora`
;; gets. The first atempt is `length*`, which counts the number of atoms in
;; `pora`

(define length*
  (lambda (pora)
    (cond
     ((atom? pora) 1)
     (else
      (plus (length* (first pora))
	    (length* (second pora)))))))

;; This however, does not pay enough attention to the first component of `pora`
;; when it is a pair:
;;
;;     scheme@(guile-user)> (length* '((a b) c))
;;     $7 = 3
;;     scheme@(guile-user)> (length* '(a (b c)))
;;     $8 = 3
;;
;; In order to mitigate that, `weight*` is presented:

(define weight*
  (lambda (pora)
    (cond
     ((atom? pora) 1)
     (else
      (plus (times (weight* (first pora)) 2)
	    (weight* (second pora)))))))

;; `weight*` shows better whether a `pora` gets simpler:
;;
;;     scheme@(guile-user)> (weight* '((a b) c))
;;     $9 = 7
;;     scheme@(guile-user)> (weight* '(a (b c)))
;;     $10 = 5
;;
;; The text concludes that `align` is in fact total, because the values of
;; `weight*` for the recursive calls to itself get smaller.
