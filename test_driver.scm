(load "scheme.scm")

(use-modules (ice-9 popen))
(use-modules (ice-9 rdelim))

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
    (display prog_data)
    (display " -> ")
    (eqv? prog_data expt)
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
    (display prog_data)
    (display " -> ")
    (equal? prog_data expt)
    ))

(prep)
(display (test "10" 10)) (newline)
(display (test "20" 20)) (newline)
(display (test "#t" #t)) (newline)
(display (test "#f" #f)) (newline)
(display (test "#\\A" #\A)) (newline)
(display (test "(add1 10)" 11)) (newline)
(display (test "(add1 200)" 201)) (newline)
(display (test "(sub1 79)" 78)) (newline)
(display (test "(fxzero? 1)" #f)) (newline)
(display (test "(fxzero? 0)" #t)) (newline)
(display (test "(fixnum? 0)" #t)) (newline)
(display (test "(fixnum? 1)" #t)) (newline)
(display (test "(fixnum? 1)" #t)) (newline)
(display (test "(null? ())" #t)) (newline)
(display (test "(boolean? #t)" #t)) (newline)
(display (test "(boolean? #f)" #t)) (newline)
(display (test "(boolean? 1)"  #f)) (newline)
(display (test "(add 1 2)"  3)) (newline)
(display (test "(sub 7 2)"  5)) (newline)
(display (test "(not #t)"  #f)) (newline)
(display (test "(not #f)"  #t)) (newline)
(display (test "(char->fixnum #\\A)"  1040)) (newline)
(display (test "(fixnum->char 1040)"   #\A)) (newline)
(display (test "(mul 9 5)" 45)) (newline)
(display (test "(mul 3 4)" 12)) (newline)

(display
 (test "(labels ((f0 (code () (x y) (add x y)))
                 (f1 (code (y) (x) (funcall (closure f0 x y) ))))
                (let ((x 5)) (funcall (closure f1 x) 3) ))" 8)
 ) (newline)

(display (test-string-output "(cons 3 4)" "(3 . 4)")) (newline)
(display (test-string-output "(cons 3 (cons 4 5))" "(3 4 . 5)")) (newline)
(display (test-string-output "(make-vector 5 5)" "#(5 5 5 5 5)")) (newline)


(display (test-string-output "(make-string 5 #\\A)"  "\"AAAAA\"")) (newline)


(last)







