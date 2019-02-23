(load "scheme.scm")

(use-modules (ice-9 popen))
(use-modules (ice-9 rdelim))


(define G "\x1b[0;32m")
(define R "\x1b[0;31m")
(define N "\x1b[0m")

(define test-number 0)

(define passed 0)

(define prep
  (lambda ()
    (system "make mk_test_dir >/dev/null 2>&1")))

(define last
  (lambda ()
    (system "make rm_test_dir >/dev/null 2>&1")))

(define write_test_to_file_port
  (lambda (port content)
    (display content port)))

(define beg_test
  (lambda ()
    (system "cd tests && make file=my_test.scm >/dev/null 2>&1")))

(define run_prog
  (lambda ()
    (system "cd tests && ./program > program.out")))

(define get_prog
  (lambda ()
    (read (open-file "tests/program.out" "r"))))

(define get_prog_as_string
  (lambda ()
    (read-line (open-file "tests/program.out" "r"))))

(define rmv_prog
  (lambda ()
    (system "cd tests; rm program.out >/dev/null 2>&1")))

(define end_test
  (lambda ()
    (system "cd tests; make clean >/dev/null 2>&1")))

(define test
  (lambda (expr expt)
    (define prog_data '())
    (let ((port (open-file "tests/my_test.scm" "w"))) (write_test_to_file_port port expr) (close-port port))
    (beg_test)
    (run_prog)
    (set! prog_data (get_prog))
    (rmv_prog)
    (end_test)
    (set! test-number (+ test-number 1))
    (when (eqv? prog_data expt) (set! passed (+ passed 1)))
    (display (if (eqv? prog_data expt)
	 (format #f "   ~aPassed~a [~a+~a] tests/~a.scm" G N G N test-number)
	 (format #f "   ~aFailed~a [~a-~a] tests/~a.scm" R N R N test-number)
	 ))
    (newline)
    ))

(define test-string-output
  (lambda (expr expt)
    (define prog_data '())
    (let ((port (open-file "tests/my_test.scm" "w"))) (write_test_to_file_port port expr) (close-port port))
    (beg_test)
    (run_prog)
    (set! prog_data (get_prog_as_string))
    (rmv_prog)
    (end_test)
    (set! test-number (+ test-number 1))
    (when (equal? prog_data expt) (set! passed (+ passed 1)))
    (display (if (equal? prog_data expt)
	 (format #f "   ~aPassed~a [~a+~a] tests/~a.scm" G N G N test-number)
	 (format #f "   ~aFailed~a [~a-~a] tests/~a.scm" R N R N test-number)
	 ))
    (newline)
    ))

(prep)

(newline)
(display "   Running tests ")
(newline)
(newline)

(test "10" 10)
(test "20" 20)
(test "#t" #t)
(test "#f" #f)
(test "#\\A" #\A)
(test "(add1 10)" 11)
(test "(add1 200)" 201)
(test "(sub1 79)" 78)
(test "(fxzero? 1)" #f)
(test "(fxzero? 0)" #t)
(test "(fixnum? 0)" #t)
(test "(fixnum? 1)" #t)
(test "(fixnum? 1)" #t)
(test "(null? ())" #t)
(test "(boolean? #t)" #t)
(test "(boolean? #f)" #t)
(test "(boolean? 1)"  #f)
(test "(add 1 2)"  3)
(test "(sub 7 2)"  5)
(test "(not #t)"  #f)
(test "(not #f)"  #t)
(test "(char->fixnum #\\A)"  1040)
(test "(fixnum->char 1040)"   #\A)
(test "(mul 9 5)" 45)
(test "(mul 3 4)" 12)
(test "(div 15 3)" 5)
(test "(div 84 2)" 42)
(test "(if #t 4 3)" 4)
(test "(if #f 4 3)" 3)
(test "(if (null? ()) 4 3)" 4)
(test "(if (null?  7) 4 3)" 3)
(test "(let ([x 3] [y 5]) (if (< x y) x y))" 3)

(test "(pair? (cons 6 7))" #t)
(test "(car (cons 6 7))" 6)
(test "(cdr (cons 6 7))" 7)
(test "(car (car (cons (cons 3 4) 7)))" 3)
(test "(cdr (car (cons (cons 3 4) 7)))" 4)
(test "(begin (add 3 4) (mul 5 6) (div 8 4))" 2)



(test "(begin (add 3 4) (div 6 33) (if #t 8 4))" 8)
(test "(let ([x (cons 3 4)] [y (cons 5 6)]) (add (car x) (car y)))" 8)
(test "(let ([x (cons 3 4)] [y (cons 5 6)]) (add (cdr x) (cdr y)))" 10)


;; TODO Use Lambda Version
;; (display (test "(labels ((f10 (code (x y) () (mul x y)))) (let ((f (closure f10))) (funcall f 3 4)))" 12)) (newline)


;; TODO Use Lambda Version
;; (display
;;  (test "(labels ((f0 (code () (x y) (add x y)))
;;                  (f1 (code (y) (x) (funcall (closure f0 x y) ))))
;;                 (let ((x 5)) (funcall (closure f1 x) 3) ))" 8)
;;  ) (newline)


(test-string-output "(cons 3 4)" "(3 . 4)")
(test-string-output "(cons 3 (cons 4 5))" "(3 4 . 5)")
(test-string-output "(make-vector 5 5)" "#(5 5 5 5 5)")

(test-string-output "(make-string 5 #\\A)"  "\"AAAAA\"")


;; (test "" expected)

(test "(let ((x 3))
  (let
      (
       (f (lambda () (x) (set! x 20))) 
       (g (lambda () (x) x))
       )
    (funcall f)
    (funcall g)))" 20)


(test-string-output "(let ([f (lambda () () (cons 3 4))]) (funcall f))" "(3 . 4)")

(test "(let ([f (lambda () () 20)] [g (lambda () () 5)]) (div (funcall f) (funcall g)))" 4)

;; 1

(test "(let ((x 3) (y 4))
  (let
      (
       (f (lambda () (x) (set! x 20))) 
       (g (lambda () (x y) (set! y x)))
       )
    (funcall f)
    (funcall g)
    (add x y)
  ))" 40)

;; 2
 (test "(let ((x 3))
  (let
      (
       (f (lambda () (x) (set! x (add x 1)))) 
       )
    (funcall f)
     x
  ))" 4)



;; 3
 (test-string-output "(let ((x (cons 3 4) ))
  (let
      (
       (f (lambda () (x) (set! x (cons (add (car x) 1) (add (cdr x) 1)) ))) 
       )
    (funcall f)
     x
  ))" "(4 . 5)")


;; 4

 (test-string-output "(let ((x (cons 3 4) ))
  (let
      (
       (f (lambda () (x) (set! x (cons (mul (car x) 5) (mul (cdr x) 5)) ))) 
       )
    (funcall f)
     x
  ))" "(15 . 20)")


;; 5

(test-string-output "(let ((v (make-string 2 #\\H) )) 
                      (let (
                           [f (lambda () (v) (set! v (make-string 2 #\\Z)))]
                           ) 
                           (funcall f)
                           v))" "\"ZZ\"") 


;; 6

(test-string-output "(let ((v (make-vector 5 5)) (i 2) (x 0)) 
                      (let (
                           [f (lambda () (v i x) (vector-set! v i x))]
                           ) 
                           (funcall f)
                           v))" "#(5 5 0 5 5)") 
 
;; 7

(test-string-output "(let ((v (make-vector 5 5)) (i 4) (x 0)) 
                      (let (
                           [f (lambda () (v i x) (vector-set! v i x))]
                           ) 
                           (funcall f)
                           v))" "#(5 5 5 5 0)") 
 
;; 8

(test-string-output "(let ((v (make-vector 5 5)) (i 0) (x 0)) 
                      (let (
                           [f (lambda () (v i x) (vector-set! v i x))]
                           ) 
                           (funcall f)
                           v))" "#(0 5 5 5 5)") 
 
;; 9

(test-string-output "(let ((v (make-string 3 #\\H) )) 
                      (let (
                           [f (lambda () (v) (set! v (make-string 10 #\\Z)))]
                           ) 
                           (funcall f)
                           v))" "\"ZZZZZZZZZZ\"") 
 
;; 10

(test "
  (let
      (
       (pro (lambda (x) () (mul x x)))
       (sum (lambda (x y) () (add x y)))
       )
    (funcall sum (funcall pro 3) (funcall pro 4))
   )" 25)





(newline)
(display (format #f "   Successfully passed ~a out of ~a tests" passed test-number )) (newline)
(newline)



(last)







