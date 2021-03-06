(cons (lambda (a b f) (f a b)))
(car  (lambda (f) (f (lambda (a b) (a)))))
(cdr  (lambda (f) (f (lambda (a b) (b)))))
(fix  (lambda (f) (let ((sub (lambda (x) (f (x x))))) (sub sub))))
(succ (lambda (n s x) (s (n s x))))
(zero (lambda (g y) (y)))
(plus (lambda (n m s x) (n s (m s x))))
(true  (lambda (t f) (t)))
(false (lambda (t f) (f)))
(if (lambda (p t f) (p t f)))
(const K)
(isZero (lambda (n) (n (const false) true)))
(times  (lambda (n m f) (n (m f))))
(pred (lambda (n)
	(cdr (n (lambda (pair)
		  (cons (cdr pair) (succ (cdr pair))))
		(cons zero zero))) ))

(not (lambda (t) (t false true)))
(double (lambda (n f) (n (lambda (x) (f (f x))))))
