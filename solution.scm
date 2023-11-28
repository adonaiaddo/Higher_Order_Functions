;;;;;;;;;;;;;;;;;;; COMP 105 HOFS ASSIGNMENT ;;;;;;;;;;;;;;;

;; functions for testing

(define even? (x) (= (mod x 2) 0))

(define member? (x s) (s x))

(define odd? (x) (= (mod x 2) 1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  Exercise 2

;; (flip f) calls the function f on the reverse order of the two given arguments

;; laws:
;; ((flip f) a b) == (f b a)

(define flip (f)
    (lambda (a b) (f b a)))

        (check-expect ((flip >) 6 2) #f)
        (check-expect ((flip <) 6 2) #t)
        (check-error (flip > 3 2))
        (check-expect ((flip append) '(a b) '(c d)) '(c d a b))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  Exercise 3

;; (takewhile f ys) returns the list of the first values in ys that satisfy the
;; function f

;; laws:
;; (takewhile f '()) == '()
;; (takewhile f (cons x xs)) == (cons x (takewhile f xs)), where (f x) evaluates
;;                              to #t
;; (takewhile f (cons x xs)) == '(), where (f x) evaluates to #f

(define takewhile (f ys)
    (if (null? ys)
        ys
        (if (f (car ys))
            (cons (car ys) (takewhile f (cdr ys)))
            '())))

        ;; unit tests

        (check-expect (takewhile even? '(2 4 6 7 8 10 12)) '(2 4 6))
        (check-expect (takewhile even? '()) '())
        (check-expect (takewhile even? '(1)) '())
        (check-expect (takewhile even? '(12)) '(12))


;; laws:
;; (dropwhile f '()) == '()
;; (dropwhile f (cons y ys)) == (cons y (takewhile f ys)), where (f y) evaluates
;;                              to #t
;; (dropwhile f (cons y ys)) == (cons y ys), where (f y) evaluates to #f

(define dropwhile (f ys)
    (if (null? ys)
        ys
        (if (f (car ys))
            (dropwhile f (cdr ys))
            ys)))

        (check-expect (dropwhile even? '(8 11 15 6)) '(11 15 6))
        (check-expect (dropwhile even? '()) '())
        (check-expect (dropwhile even? '(10)) '())


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  Exercise 4

;; (ordered-by f) returns a boolean #t when successive elements in a given list
;; ys, return #t when passed into f, otherwise, it returns #f

;; laws:
;; ((ordered-by f) '()) == #t
;; ((ordered-by f) '(y)) == #t
;; ((ordered-by f) (cons y ys)) == (&& (f y car(ys)) ((ordered-by f) ys))

(define ordered-by (f)
    (lambda (ys)
        (if (null? ys)
            #t
            (if (null? (cdr ys))
                #t
                (&& (f (car ys) (cadr ys)) ((ordered-by f) (cdr ys)))))))


        (check-expect ((ordered-by >) '()) #t)
        (check-expect ((ordered-by <) '(9)) #t)
        (check-expect ((ordered-by =) '(10 100)) #f)
        (check-expect ((ordered-by <) '(1 2 8 9)) #t)
        (check-expect ((ordered-by <) '(4 5 3 9 10)) #f)
        (check-error (ordered-by = '(1 2 3)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  Exercise 5

;; (max* ys) returns the max value in a given non-empty list ys

(define max* (ys)
    (foldl max (car ys) (cdr ys)))

        (check-expect (max* '(-15 0)) 0)
        (check-expect (max* '(-3 -2 -9 -20)) -2)
        (check-expect (max* '(1 2 3 4 5 2)) 5)
        (check-error (max* '()))


;; (sum ys) returns the sum of all values in a non-empty list ys
(define sum (ys)
    (foldl + (car ys) (cdr ys)))

        (check-expect (sum '(-1)) -1)
        (check-expect (sum '(0)) 0)
        (check-expect (sum '(5 3 9)) 17)
        (check-expect (sum '(8 -11)) -3)
        (check-error (sum '()))


;; (product ys) returns the product of all the elements in the list ys

(define product (ys)
    (foldl * (car ys) (cdr ys)))

        (check-expect (product '(0 1 1000)) 0)
        (check-expect (product '(-1)) -1)
        (check-expect (product '(1 5 2)) 10)
        (check-expect (product '(2 -8 -3 -9)) -432)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  Exercise 6

;; (append xs ys) returns a list of elements in ys appended to elements in xs

(define append (xs ys)
    (foldr cons ys xs))

        (check-expect (append '() '()) '())
        (check-expect (append '() '(0)) '(0))
        (check-expect (append '(0) '()) '(0))
        (check-expect (append '(0) '(0)) '(0 0))
        (check-expect (append '(1 2) '()) '(1 2))
        (check-expect (append '() '(1 2)) '(1 2))
        (check-expect (append '(a b c) '(d e)) '(a b c d e))
        (check-expect (append '(1) '(2)) '(1 2))


(define reverse (xs)
    (foldl cons '() xs))

        (check-expect (reverse '(1 2 3 4 5)) '(5 4 3 2 1))
        (check-expect (reverse '(1 0)) '(0 1))
        (check-expect (reverse '(10)) '(10))
        (check-expect (reverse '()) '())


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  Exercise 7

;; (map f ys) applies the function f to every element in the list ys

(define map (f ys)
    (foldr (lambda (a b) (cons (f a) b)) '() ys))
        
    (define square (x) (* x x))
        (check-expect (map square '(1 2 3 4 5)) '(1 4 9 16 25))
        (check-expect (map square '()) '())
        (check-expect (map square '(8)) '(64))


;; (filter f ys) returns a list of the elements in ys that return #t when the
;; function f is applied to them, in the order in which they occur in ys

(define filter (f ys)
    (foldr (lambda (a b) (if (f a) (cons a b) b)) '() ys))

        (check-expect (filter even? '(1 2 3 4 5 6 7 8 9 10)) '(2 4 6 8 10))
        (check-expect (filter even? '(9)) '())
        (check-expect (filter odd? '(1 2 3 4 5 6 7 8 9 10)) '(1 3 5 7 9))
        (check-expect (filter even? '()) '())
        (check-expect (filter odd? '()) '())


;; (exists? f ys) returns a boolean #t if any value y in ys returns #t when
;; the function f is applied to it: (= (f y) #t), otherwise, it returns #f

(define exists? (f ys)
    (foldr (lambda (x y) (|| (f x) y)) #f ys))

        (check-assert (exists? even? '(2)))
        (check-assert (not(exists? even? '())))
        (check-assert (not(exists? even? '(3 5 11))))
        (check-assert (exists? even? '(1 0 5)))
        (check-assert (exists? odd? '(3 8 5 6)))
        (check-assert (not(exists? odd? '(2 4 6 8))))


;; (all? f ys) returns a boolean value #t if all the values in ys return #t when
;; the function f is applied to each one of them, otherwise it returns #f

(define all? (f ys)
    (foldr (lambda (a b) (&& (f a) b)) #t ys))

        (check-assert (not(all? even? '(2 4 6 8 9))))
        (check-assert (all? even? '(2 4 6 8 10)))
        (check-assert (all? odd? '(1)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  Exercise 8

;; evens holds a function which validates all even numbers
(val evens (lambda (x) (= (mod x 2) 0)))

        (check-assert (member? 0 evens))
        (check-assert (member? 2109457018 evens))
        (check-assert (not(member? 3 evens)))

;; two-digits holds a function which validates all two-digit numbers
(val two-digits (lambda(x)
                    (&& (&& (> x 0) (< (/ x 10) 10)) (> (/ x 10) 0))))

        (check-assert (member? 11 two-digits))
        (check-assert (not(member? 1 two-digits)))
        (check-assert (not(member? 111 two-digits)))


;; (add-element x s) adds the element x into the set s by validating the element
;; x or the set s
;; (member? x (add-element x s)) should return #t
;; (member? x (add-element y s)), where (not (equal? y x)) should be #f

(define add-element (x s)
    (lambda (y) (|| (= y x) (s y))))

        (check-assert (member? 1 (add-element 1 evens)))
        (check-assert (not(member? 3 (add-element 5 evens))))
        (check-assert (member? 5 (add-element 5 two-digits)))
        (check-assert (member? 2 (add-element 2 evens)))
        (check-assert (member? 13 (add-element 13 two-digits)))


;; (union s1 s2) unions the two sets s1 and s2 by validating whether a given
;; element is either in s1 or s2
;; (member? x (union s1 s2)) should be #t if x is in s1, s2, or both

(define union (s1 s2)
    (lambda (y) (|| (s1 y) (s2 y))))

        (check-assert (member? 11 (union two-digits evens)))
        (check-assert (not(member? 3 (union two-digits evens))))
        (check-assert (member? 44 (union two-digits evens)))
        (check-assert (member? 2 (union two-digits evens)))


;; (inter s1 s2) creates an intersection function of the sets s1 and s2 by 
;; checking if a given element y is in both
;; (member? x (inter s1 s2)) should be #t only if x is in both s1 and s2

(define inter (s1 s2)
    (lambda (y) (&& (s1 y) (s2 y))))

    (check-assert (not(member? 122 (inter two-digits evens))))
    (check-assert (not(member? 2 (inter two-digits evens))))
    (check-assert (not(member? 13 (inter two-digits evens))))
    (check-assert (member? 98 (inter two-digits evens)))


;; (diff s1 s2) creates the difference set of s1 and s2 by checking whether an
;; element is in s1 but not in s2
;; (member? x (diff s1 s2)) should be #t only if x is in s1 and not in s2

(define diff (s1 s2)
    (lambda (x) (&& (s1 x) (not (s2 x)))))

    (check-assert (member? 13 (diff two-digits evens)))
    (check-assert (not(member? 98 (diff two-digits evens))))
    (check-assert (not(member? 100 (diff two-digits evens))))
    (check-assert (not(member? 5 (diff two-digits evens))))