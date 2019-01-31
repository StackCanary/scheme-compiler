(import (rnrs arithmetic bitwise (6)))
(import (srfi srfi-1))

					; Emit 

(define (emit . args) (display (apply format #f args)) (newline))

					; Binary Op -- TODO use logior, logiand

(define shift-r bitwise-arithmetic-shift-right)
(define shift-l bitwise-arithmetic-shift-left )
(define (booltoi x) (if x 1 0))

					; Predicates

(define (immediate? x)
  (or (integer? x) (char? x) (boolean? x) (null? x)))

(define (primitive? x)
  (and (symbol? x)
       (case x
	 [(add1)         #t]
	 [(sub1)         #t]
	 [(char->fixnum) #t]
	 [(fixnum->char) #t]
	 [(fxzero?)      #t]
	 [(null?)        #t]
	 [(not)          #t]
	 [(fixnum?)      #t]
	 [(boolean?)     #t]
	 [(add)          #t]
	 [(sub)          #t]
	 [(mul)          #t]
	 [(div)          #t]
	 [(if)           #t]
	 [(<)            #t]
	 [(=)            #t]
	 [(>)            #t]
	 [(pair?)        #t]
	 [(cons)         #t]
	 [(car)          #t]
	 [(cdr)          #t]
	 [(make-vector)  #t]
	 [(vector-set!)  #t]
	 [(vector-ref)   #t]
	 [(make-string)  #t]
	 [ else          #f])))

(define (is-labelled? expr tag) (and (pair? expr) (eqv? (car expr) tag)))

(define (let? x) (is-labelled? x 'let)) 
(define (let*? x) (is-labelled? x 'let*))
(define (begin? x) (is-labelled? x 'begin))
(define (lambda? x) (is-labelled? x 'lambda))
(define (code? x) (is-labelled? x 'code))
(define (labels? x) (is-labelled? x 'labels))
(define (labelcall? x) (is-labelled? x 'labelcall))
(define (funcall? x) (is-labelled? x 'funcall))
(define (closure? x) (is-labelled? x 'closure))
(define (set? x) (is-labelled? x 'set!))


(define variable? symbol?)

(define (bindings x) (car (cdr x)))
(define (body x) (cddr x))

(define (primcall-emitter x)
  (case x
    [(add1)         add1-primcall-emitter]
    [(sub1)         sub1-primcall-emitter]
    [(char->fixnum) char->fixnum-primcall-emitter]
    [(fixnum->char) fixnum->char-primcall-emitter]
    [(fxzero?)      fxzero?-primcall-emitter]
    [(null?)        null?-primcall-emitter]
    [(fixnum?)      fixnum?-primcall-emitter]
    [(pair?)        pair?-primcall-emitter]
    [(closure?)     closure?-primcall-emitter]
    [(symbol?)      symbol?-primcall-emitter]
    [(vector?)      vector?-primcall-emitter]
    [(string?)      string?-primcall-emitter]
    [(boolean?)     boolean?-primcall-emitter]
    [(not)          not-primcall-emitter]
    [(add)          add-primcall-emitter]
    [(sub)          sub-primcall-emitter]
    [(mul)          mul-primcall-emitter]
    [(div)          div-primcall-emitter]
    [(if)           if-primcall-emitter]
    [(<)            lt-primcall-emitter]
    [(=)            eq-primcall-emitter]
    [(>)            gt-primcall-emitter]
    [(cons)         cons-primcall-emitter]
    [(car)          car-primcall-emitter]
    [(cdr)          cdr-primcall-emitter]
    [(make-vector)  make-vector-primcall-emitter]
    [(vector-set!)  vector-set-primcall-emitter]
    [(vector-ref)   vector-ref-primcall-emitter]
    [(make-string)  make-string-primcall-emitter]
    ))

;;  not,  boolean?,
;; and char?.

(define (primcall? x) (and (pair? x) (primitive? (car x))))

(define glb-label 10)

(define (get-label)
  (set! glb-label (+ 1 glb-label))
  (format #f "var~a" (- glb-label 1)))

(define (lab-label)
  (set! glb-label (+ 1 glb-label))
  (format #f "lab~a" (- glb-label 1)))

(define (fun-label)
  (set! glb-label (+ 1 glb-label))
  (format #f "fun~a" (- glb-label 1)))

(define (lvr-label)
  (set! glb-label (+ 1 glb-label))
  (string->symbol (format #f "f~a" (- glb-label 1))))

(define (add1-primcall-emitter env arg)
  (let ((label1 (get-label)) (label2 (get-label)))
    (emit-expr arg env)
    (emit "%~a = load i64, i64* %tmp" label1)
    (emit "%~a = add i64 %~a, 4"      label2 label1)
    (emit "store i64 %~a, i64* %tmp"  label2)))

(define (sub1-primcall-emitter env arg)
  (let ((label1 (get-label)) (label2 (get-label)))
    (emit-expr arg env)
    (emit "%~a = load i64, i64* %tmp" label1)
    (emit "%~a = sub i64 %~a, 4"      label2 label1)
    (emit "store i64 %~a, i64* %tmp"  label2)))

(define (char->fixnum-primcall-emitter env arg)
  (let ((label1 (get-label)) (label2 (get-label)) (label3 (get-label)))
    (emit-expr arg env)
    (emit "%~a = load i64, i64* %tmp" label1)
    (emit "%~a = ashr i64 %~a, 4"     label2 label1)
    (emit "%~a = shl  i64 %~a, 2"     label3 label2)
    (emit "store i64 %~a, i64* %tmp"  label3)))

(define (fixnum->char-primcall-emitter env arg)
  (let ((label1 (get-label)) (label2 (get-label)) (label3 (get-label)))
    (emit-expr arg env)
    (emit "%~a = load i64, i64* %tmp" label1)
    (emit "%~a = shl i64 %~a, 2"      label2 label1)
    (emit "%~a = or  i64 %~a, 15"     label3 label2)
    (emit "store i64 %~a, i64* %tmp"  label3)))

(define (fxzero?-primcall-emitter env arg)
  (let ((label1 (get-label)) (label2 (get-label))
	(label3 (get-label)) (label4 (get-label))
	(label5 (get-label)))
    (emit-expr arg env)
    (emit "%~a = load i64, i64* %tmp" label1)
    (emit "%~a = icmp eq i64 %~a, 0"  label2 label1)
    (emit "%~a = zext i1 %~a to i64"  label3 label2)
    (emit "%~a = shl i64 %~a, 7"      label4 label3)
    (emit "%~a = or i64 %~a, 31"      label5 label4)
    (emit "store i64 %~a, i64* %tmp"  label5)))


(define (null?-primcall-emitter env arg)
  (let ((label1 (get-label)) (label2 (get-label))
	(label3 (get-label)) (label4 (get-label))
	(label5 (get-label)))
    (emit-expr arg env)
    (emit "%~a = load i64, i64* %tmp" label1)
    (emit "%~a = icmp eq i64 %~a, 47" label2 label1)
    (emit "%~a = zext i1 %~a to i64"  label3 label2)
    (emit "%~a = shl i64 %~a, 7"      label4 label3)
    (emit "%~a = or i64 %~a, 31"      label5 label4)
    (emit "store i64 %~a, i64* %tmp"  label5)))

(define (last-3-bits-eq env arg bits)
  (let ((label1 (get-label)) (label2 (get-label))
	(label3 (get-label)) (label4 (get-label))
	(label5 (get-label)) (label6 (get-label)))
    (emit-expr arg env)
    (emit "%~a = load i64, i64* %tmp" label1)
    (emit "%~a = and i64 %~a, 3"      label2 label1) ;; TODO FIX
    (emit "%~a = icmp eq i64 %~a, ~a" label3 label2 bits)
    (emit "%~a = zext i1 %~a to i64"  label4 label3)
    (emit "%~a = shl i64 %~a, 7"      label5 label4)
    (emit "%~a = or i64 %~a, 31"      label6 label5)
    (emit "store i64 %~a, i64* %tmp"  label6)))

(define (fixnum?-primcall-emitter env arg)
  (last-3-bits-eq env arg 0))

(define (pair?-primcall-emitter env arg)
  (last-3-bits-eq env arg 1))

(define (closure?-primcall-emitter env arg)
  (last-3-bits-eq env arg 2))

(define (symbol?-primcall-emitter env arg)
  (last-3-bits-eq env arg 3))

(define (vector?-primcall-emitter env arg)
  (last-3-bits-eq env arg 5))

(define (string?-primcall-emitter env arg)
  (last-3-bits-eq env arg 6))

(define (boolean?-primcall-emitter env arg)
  (let ((label1 (get-label)) (label2 (get-label))
	(label3 (get-label)) (label4 (get-label))
	(label5 (get-label)) (label6 (get-label)))
    (emit-expr arg env)
    (emit "%~a = load i64, i64* %tmp" label1)
    (emit "%~a = and i64 %~a, 127"    label2 label1)
    (emit "%~a = icmp eq i64 %~a, 31" label3 label2) 
    (emit "%~a = zext i1 %~a to i64"  label4 label3)
    (emit "%~a = shl i64 %~a, 7"      label5 label4)
    (emit "%~a = or i64 %~a, 31"      label6 label5)
    (emit "store i64 %~a, i64* %tmp"  label6)))

(define (not-primcall-emitter env arg)
  (let ((label1 (get-label)) (label2 (get-label)))
    (emit-expr arg env)
    (emit "%~a = load i64, i64* %tmp" label1)
    (emit "%~a = xor i64 %~a, 128"    label2 label1)
    (emit "store i64 %~a, i64* %tmp"  label2)))

(define (add-primcall-emitter env arg1 arg2)
  (let ((label1 (get-label)) (label2 (get-label))
	(label3 (get-label)))
    (emit-expr arg2 env)
    (emit "%~a = load i64, i64* %tmp" label1)
    (emit-expr arg1 env)
    (emit "%~a = load i64, i64* %tmp" label2)
    (emit "%~a = add i64 %~a, %~a"    label3 label2 label1)
    (emit "store i64 %~a, i64* %tmp"  label3)))

(define (sub-primcall-emitter env arg1 arg2)
  (let ((label1 (get-label)) (label2 (get-label))
	(label3 (get-label)))
    (emit-expr arg2 env)
    (emit "%~a = load i64, i64* %tmp" label1)
    (emit-expr arg1 env)
    (emit "%~a = load i64, i64* %tmp" label2)
    (emit "%~a = sub i64 %~a, %~a"    label3 label2 label1)
    (emit "store i64 %~a, i64* %tmp"  label3)))

(define (mul-primcall-emitter env arg1 arg2)
  (let ((label1 (get-label)) (label2 (get-label))
	(label3 (get-label)) (label4 (get-label)))
    (emit-expr arg2 env)
    (emit "%~a = load i64, i64* %tmp" label1)
    (emit-expr arg1 env)
    (emit "%~a = load i64, i64* %tmp" label2)
    (emit "%~a = mul  i64 %~a, %~a"    label3 label2 label1)
    (emit "%~a = sdiv i64 %~a,   4"    label4 label3)
    (emit "store i64 %~a, i64* %tmp"  label4)))

(define (div-primcall-emitter env arg1 arg2)
  (let ((label1 (get-label)) (label2 (get-label))
	(label3 (get-label)))
    (emit-expr arg2 env)
    (emit "%~a = load i64, i64* %tmp" label1)
    (emit-expr arg1 env)
    (emit "%~a = load i64, i64* %tmp" label2)
    (emit "%~a = sdiv i64 %~a, %~a"    label3 label2 label1)
    (emit "store i64 %~a, i64* %tmp"  label3)))

(define (emit-label label)
  (emit "~a:" label))

(define (if-primcall-emitter env test conseq altern)
  (let ((L1 (lab-label))
	(L2 (lab-label))
	(L3 (lab-label))
	(label1 (get-label))
	(label2 (get-label)))
    (emit-expr test   env) ;
    (emit "%~a = load i64, i64* %tmp"  label1)
    (emit "%~a = icmp eq i64 %~a, ~a"  label2 label1 (immediate-rep #t))
    (emit "br i1 %~a, label %~a, label %~a " label2 L1 L2)
    (emit-label L1)
    (emit-expr conseq env)
    (emit "br label %~a " L3)
    (emit-label L2)
    (emit-expr altern env)
    (emit "br label %~a " L3)
    (emit-label L3)
    ))

(define (lt-primcall-emitter env arg1 arg2)
  (let ((label1 (get-label))
	(label2 (get-label))
	(label3 (get-label))
	(label4 (get-label))
	(label5 (get-label))
	(label6 (get-label))
	)
    (emit-expr arg2 env)
    (emit "%~a = load i64, i64* %tmp" label1)
    (emit-expr arg1 env)
    (emit "%~a = load i64, i64* %tmp" label2)
    (emit "%~a = icmp slt i64 %~a, %~a" label3 label2 label1) 
    (emit "%~a = zext i1 %~a to i64"  label4 label3)
    (emit "%~a = shl i64 %~a, 7"      label5 label4)
    (emit "%~a = or i64 %~a, 31"      label6 label5)
    (emit "store i64 %~a, i64* %tmp"  label6)
    ))

(define (eq-primcall-emitter env arg1 arg2)
  (let ((label1 (get-label))
	(label2 (get-label))
	(label3 (get-label))
	(label4 (get-label))
	(label5 (get-label))
	(label6 (get-label))
	)
    (emit-expr arg2 env)
    (emit "%~a = load i64, i64* %tmp" label1)
    (emit-expr arg1 env)
    (emit "%~a = load i64, i64* %tmp" label2)
    (emit "%~a = icmp eq i64 %~a, %~a" label3 label2 label1) 
    (emit "%~a = zext i1 %~a to i64"  label4 label3)
    (emit "%~a = shl i64 %~a, 7"      label5 label4)
    (emit "%~a = or i64 %~a, 31"      label6 label5)
    (emit "store i64 %~a, i64* %tmp"  label6)
    ))

(define (gt-primcall-emitter env arg1 arg2)
  (let ((label1 (get-label))
	(label2 (get-label))
	(label3 (get-label))
	(label4 (get-label))
	(label5 (get-label))
	(label6 (get-label))
	)
    (emit-expr arg2 env)
    (emit "%~a = load i64, i64* %tmp" label1)
    (emit-expr arg1 env)
    (emit "%~a = load i64, i64* %tmp" label2)
    (emit "%~a = icmp sgt i64 %~a, %~a" label3 label2 label1) 
    (emit "%~a = zext i1 %~a to i64"  label4 label3)
    (emit "%~a = shl i64 %~a, 7"      label5 label4)
    (emit "%~a = or i64 %~a, 31"      label6 label5)
    (emit "store i64 %~a, i64* %tmp"  label6)
    ))

(define (and-primcall-emitter env arg1 arg2)
  '()
  )

(define (or-primcall-emitter env arg1 arg2)
  '()
  )

(define (emit-begin x env)
  (cond
   ((null? x) '())
   ( else (begin (emit-expr (car x) env) (emit-begin (cdr x) env)))))


(define (emit-let-help b* new-env body e)
  (cond ((null? b*) (emit-begin body new-env))
	( else	 (let ((b (car b*)) (label (get-label)))
		   (emit-expr (cadr b) e)
		   (emit "%~a = load i64, i64* %tmp" label)
		   (emit-let-help (cdr b*) (envput (car b) label #f 0 new-env) body e)
		   ))))

(define (emit-let bindings body env)
  (emit-let-help bindings env body env))


(define (transform-let* bindings body env)
  (cond
   ((null? bindings) body)
   ( else (let ((b (car bindings)) (b* (cdr bindings)))
	    (if (null? b*)
		(cons 'let (cons (cons b '()) body))
		(cons 'let (cons (cons b '()) (cons (transform-let* b* body env) '())))
		)))
   ))

;; TODO Implement Lambdas


					; Emit label expression (label ([lvar code] ..) expr)
(define (emit-labels expr)
  
					; Create a new environment mapping function names (lvars) to unique labels
  
  (define (make-env b)
    (cond
     ((null? b) '())
     ( else      (envput (caar b) (fun-label) #f (length (car (cdadar b))) (make-env (cdr b)))) ;; Populate env with lvar -> labels not-free-var func-arg-count
     ))
  

  (define (expr-bin e)
    (cadr e))

  (define (expr-bod e)
    (caddr e))

  (define (code-var c)
    (cadr c))

  (define (code-fvr c)
    (caddr c))

  (define (code-exp c)
    (cadddr c))

  ;; 
  (let* ((bindings (expr-bin expr)) (env (make-env bindings)))
					; Emit functions for each binding [lvar code]
    
    (for-each 					
     (lambda (binding)
       (let* ((lvar (car binding)) (code (cadr binding)) (var (code-var code)) (fvr (code-fvr code)) (expr (code-exp code)) (labl (lookup-env-val lvar env)))
	 (emit-code labl var fvr expr env)
	 )) bindings)
    
    ;; Emit Scheme Entry for expr
    (emit-code "scheme_entry" '() '()  (expr-bod expr) env))
  )


(define (emit-code labl var fvr expr env)   ; labl - func name, var - list of formal func args, fvr - free vars, expr - body
  
  (emit-header labl (length var))	; Emit Function Header
  
  ;; Extend env with form variables
  ;; Extend env with free variables
  ;; Emit Body with Extended Environment
  (let extend-var ((var var) (fvr fvr) (var-arg 0) (fvr-arg 0) (env env))
    (cond
     ((and (null? var) (null? fvr))
      (emit-expr expr env)) 
     ((pair? var)
      (extend-var (cdr var) fvr (+ var-arg 1) fvr-arg (envput (car var) (format #f "arg~a" var-arg) #f 0 env)))
     ((pair? fvr)
      (extend-var var (cdr fvr) var-arg (+ fvr-arg 1) (envput (car fvr) (format #f    "~a" fvr-arg) #t 0 env)))
     ))

  (emit-footer))			; Emit Function Footer


(define (comma-interpersed-list list)
  (cond
   ((eqv? (length list) 0) "")
   ((eqv? (length list) 1) (format #f "i64 %~a" (car list)))
   ( else (string-append (format #f "i64 %~a, " (car list)) (comma-interpersed-list (cdr list))))
   ))

(define (emit-labelcall lvar exprs env)
  (define (emit-args exprs arg-vars)
    (cond 
     ((null? exprs) (emit "call i64 @~a(~a)" (lookup-env-val lvar env) (comma-interpersed-list (reverse arg-vars))))
     ( else
       (let ((label (get-label)))
	 (emit-expr (car exprs) env)
	 (emit "%~a = load i64, i64* %tmp" label)
	 (emit-args (cdr exprs) (cons label arg-vars))
	 )
       )
     )
    )

  (emit-args exprs '()))

;; Set and Restore the Closure Pointer 
(define (emit-funcall fun-expr arg-exprs env)
  
  (define (emit-args a-exprs a-vars)
    (cond 
     ((null? a-exprs)
      (let ((clsptr (get-label)) (funptr (get-label)) (funptr_cast (get-label)) (prev_clsptr (get-label)) (retval (get-label)))
	(emit-expr fun-expr env) ;; Put Closure Ptr in Tmp
	(emit "%~a = load i64, i64* %tmp" clsptr) ;; Load New Closure Ptr
	(emit "%~a = call i64 @hptr_get_clsptr()" prev_clsptr) ;; Save Prev Clsr Ptr
	(emit "call void @hptr_set_clsptr(i64 %~a)" clsptr) ;; Set New Clsr Ptr
	(emit "%~a = call i64 @hptr_closure_lab()" funptr) ;; Load FunPtr from ClsPtr
	(emit "%~a = inttoptr i64 %~a to ~a" funptr_cast funptr (emit-fcnptr (length arg-exprs))) ;; Cast from long to function pointer
	(emit "%~a = call i64 %~a(~a)" retval funptr_cast  (comma-interpersed-list (reverse a-vars))) ;; Call FunPtr
	(emit "call void @hptr_set_clsptr(i64 %~a)" prev_clsptr) ;;  Restore Prev Clsr Ptr
	(emit "store i64 %~a, i64* %tmp" retval)       
	)
      )
     ( else
       (let ((label (get-label)))
	 (emit-expr (car a-exprs) env)
	 (emit "%~a = load i64, i64* %tmp" label)
	 (emit-args (cdr a-exprs) (cons label a-vars)))
       )
     )
    )

  (emit-args arg-exprs '()))

(define (emit-fcnptr count)

  (define (f c)
    (cond
     ((eqv? c 0) "")
     ((eqv? c 1) (string-append "i64" (f (- c 1))))
     ( else (string-append "i64, " (f (- c 1))))))
  
  (format #f "i64 (~a)*" (f count)))

;; Emit Closure Object on heap, I think I need to store the function ptr and then the values of the free variables
(define (emit-closure lvar fvar env)
  (let ((label1 (get-label)) (label2 (get-label))
	(label3 (get-label)) (label4 (get-label)))

    ;; Save hptr 
    (emit "%~a = call i64 @hptr_ptr(i64 2)" label1) ;; Get Heap Ptr for Storage on Stack

    ;; Store Length of Free Variables
    (emit "call void @hptr_inc(i64 ~a)" (length fvar))     
    
    ;; Store Function Ptr
    (emit "call void @hptr_inc(i64 ptrtoint (~a @~a to i64))" (emit-fcnptr (lookup-env-fun-arg lvar env)) (lookup-env-val lvar env)) ;; Store Fcn ptr on heap

    ;; Store Free Variables into Heap
    (for-each
     (lambda (fv)
       (let ((fvar-label (get-label)))
	 (emit-variable fv env)
	 (emit "%~a = load i64, i64* %tmp" fvar-label)
	 (emit "call void @hptr_inc(i64 %~a)" fvar-label)     
	 )
       )
     fvar)

    ;; Emit Ptr to Closure Object we just created
    (emit "store i64 %~a, i64* %tmp" label1)         ;; 
    ))

;; TODO
;; Given a lambda return the free variables
(define (free-vars expr)
  '())

;; Annotate lambdas with their free variables
;; TODO
(define (transform_a expr)
  (cond
   [(  null? expr) expr]

   ))

(define (lhs x) (car x))
(define (rhs x)(cadr x))

;; Transform lambdas annoted with free variables into labels, code and closure forms
;; TODO
(define (transform_b expr)

  (define (mk-code  var fvr body)
    (cons*    'code var fvr body))
  
  (define (mk-clos lvar fvr)
    (cons* 'closure lvar fvr))

  ;; Top Level Labels
  (define labels '())


  (define (transform-let-bindings bindings)

    (map

     (lambda (binding)

       (let ((lhs (lhs binding)) (rhs (rhs binding)))

	 (list lhs (transform rhs))
	 
	 )
       )

     bindings)
    
    )
  
  (define (transform x)
    (cond
     [(     null? x) x]
     [(   lambda? x) (let* ((var (cadr x)) (fvr (caddr x)) (body (cdddr x)) (lvar (lvr-label)) (body (transform body)))
		       
		       ;; Put code into labels, continue transformation on body and Return Closure
		       (set! labels
			     (cons (list lvar (mk-code var fvr body)) labels))

		       (mk-clos lvar fvr) ;; Change Lambda to Closure
		       
		       )
      ] ;; TODO (cadr) - var, (caddr) - fvr, (cdddr) - body
     [(immediate? x) x]
     [( primcall? x) (cons* (car x) (transform (cdr x)))] 
     [( variable? x) x]
     [(      let? x) (cons* 'let   (transform-let-bindings (bindings x)) (transform (body x)))] ;; TODO tranform bindings
     [(     let*? x) (cons* 'let*  (transform-let-bindings (bindings x)) (transform (body x)))] ;; TODO transform bindings
     [(    begin? x) (cons* 'begin (transform (cdr x)))] ;; TODO (cdr x)
     [(     list? x) (map transform x)]
     [       else    (error "Unimplemented transformation in transform_b")]))


  (let ((transformed-expr (transform expr)))

    (list 'labels labels transformed-expr)
    
    )
  )


(define (mk-lambda var fvr body)
  (cons* 'lambda var fvr body)
  )

;; TODO Variable Transformation
;; Citation https://github.com/namin/inc/
(define (transform_c expr)

  (define assignable '())

  ;; TODO Double Check
  (define (assignable? variable)
    (member variable assignable))

  ;; TODO Double Check
  (define (make-assignable variable)
    (unless (assignable? variable)
      (set! assignable (cons variable assignable))))

  ;; TODO Double Check  
  (define (find-assignable expr)
    (when (set? expr)
      (make-assignable (cadr expr))
      )
    (when (list? expr)
      (for-each find-assignable expr))
    )

  (define (mk-let bindings body)
    (cons* 'let   bindings body)
    )

  ;; TODO Double Check  
  (define (transform expr)
    (cond
     [(   set? expr)
      (list 'vector-set! (cadr expr) 0 (transform (caddr expr)))
      ]
     
     [(lambda? expr)
      (let ((a-vars (filter assignable? (cadr expr))))
	(cond
	 [(null? a-vars)
	  (mk-lambda (cadr expr) (caddr expr) (transform (cdddr expr)))
	  ]
	 [ else
	   (mk-lambda (cadr expr)
		      (caddr expr)
		      (list (mk-let
			     (map (lambda (v) (list v (list 'make-vector 1 v))) a-vars)
			     (transform (cdddr expr))
			     )))
	   ]
	 )
	)
      ]
     
     [(   let? expr) ;; TODO
      (mk-let
       (map (lambda (binding)
              (let ([var (car binding)]
                    [val (transform (cadr binding))])
                (list var
                      (if (assignable? var)
			  (list 'make-vector 1 val)
                          val))))
            (cadr expr))
       (transform (cddr expr)))
      ]

     [(  list? expr)
      (map transform expr)
      ]

     [(and (variable? expr) (assignable? expr))
      (list 'vector-ref expr 0)
      ]

     [else expr]
     )
    )

  (find-assignable expr)

  (transform expr)
  )

(define (all? x)
  (cond
   ((null? x) #t)
   ( else
     (if (car x) (all? (cdr x)) #f)
     )))

;; The Interpreter
(define (transform_e expr)

  (define (transform-primargs primargs)
    (map
     (lambda (arg)
       (if (immediate? arg) arg (transform arg)))
     primargs)
    )


  (define (transform-let-bindings bindings)
    (map
     (lambda (binding)
       (let ((lhs (lhs binding)) (rhs (rhs binding)))
	 (list lhs (transform rhs))
	 )
       )

     bindings)
    )

  (define (primcall-optimiser primcall primargs)
    (case primcall
      [( add) (apply + primargs)]
      [( sub) (apply - primargs)]
      [( mul) (apply * primargs)]
      [( div) (apply / primargs)]
      [ else (cons primcall primargs)]
     )
    )

  (define (transform x)
    (cond
     [(     null? x) x] ;; Done
     [(   lambda? x) (mk-lambda (cadr x) (caddr x) (transform (cdddr x)))]
     [(immediate? x) x] ;; Done
     
     [( primcall? x) 

       (let* ([primcall (car x)]
	      [primargs (cdr x)]
	      [transformed-primargs (transform-primargs primargs)] ;; Transform Children
	      [all-immediates (all? (map immediate? transformed-primargs))])
	 
	 (if
	  all-immediates
	  (primcall-optimiser primcall transformed-primargs)      ;; Then Transform ourselves
	  (cons primcall transformed-primargs)
	  )
	 
	 )
      
      ]
     
     [( variable? x) x] ;; Done
     [(      let? x) (cons* 'let   (transform-let-bindings (bindings x)) (transform (body x)))] ;; Done
     [(     let*? x) (cons* 'let*  (transform-let-bindings (bindings x)) (transform (body x)))] ;; Done
     [(    begin? x) (cons* 'begin (transform (cdr x)))] ;; Done
     [(     list? x) (map transform x)] ;; Done
     [       else    x])) ;; Done

  
  (transform expr)
  )

;; Constant Propogation
;; Replace known variable values with their symbols
;; Apply (interpreter transform) transform_e to rhs of let bindings
(define (transform_d expr)
  
  (define (transform-let-bindings bindings)
    (map
     (lambda (binding)
       (let ((lhs (lhs binding)) (rhs (rhs binding)))
	 (list lhs (transform (transform_e rhs)))
	 )
       )
     
     bindings)
    )

  (define (transform x)
    (cond
     [(     null? x) x] ;; Done
     [(   lambda? x) (mk-lambda (cadr x) (caddr x) (transform (cdddr x)))]
     [(immediate? x) x] ;; Done
     [( primcall? x) x]
     [( variable? x) x] ;; Done
     [(      let? x) (cons* 'let   (transform-let-bindings (bindings x)) (transform (body x)))] ;; Done
     [(     let*? x) (cons* 'let*  (transform-let-bindings (bindings x)) (transform (body x)))] ;; Done
     [(    begin? x) (cons* 'begin (transform (cdr x)))] ;; Done
     [(     list? x) (map transform x)] ;; Done
     [       else    x])) ;; Done

  (transform expr)

  )

(define (emit-primcall expr env)
  (let ((p (car expr))
	(a (cdr expr)))
    (apply (primcall-emitter p) (cons env a))))

(define (immediate-rep x)
  (cond
   ((integer? x) (shift-l x 2))
   ((char?    x) (logior (shift-l (char->integer x) 8) #x0F))
   ((boolean? x) (logior (shift-l (booltoi x)       7) #x1F))
   ((null?    x)  47)
   ))

;; (key-> value is-free func-arg)

(define (lookup x env) (cdr (assv x env)))

(define (envput x val free args env) (cons (list x val free args) env))

(define (lookup-env-val x env)
  (car (lookup x env)))

(define (lookup-is-free x env)
  (cadr (lookup x env)))

(define (lookup-env-fun-arg x env)
  (caddr (lookup x env)))

(define (emit-variable expr env)
  (let* ((tuple (lookup expr env)) (free (cadr tuple)) (value (car tuple)) (funarg (caddr tuple)))
    (if free
	(emit "call i64 @hptr_get_freevar(i64 ~a, i64* %tmp)" value) ;; Get freevar from closure pointer
	(emit "store i64 %~a, i64* %tmp" value)
	)))

;; Emit Expression into a register
(define (emit-expr x env)
  (cond
   ((immediate? x) (emit "store i64 ~a, i64* %tmp" (immediate-rep x) ))
   (( primcall? x) (emit-primcall x env))
   (( variable? x) (emit-variable x env)) 
   ((      let? x) (emit-let  (bindings x) (body x) env))
   ((     let*? x) (emit-expr (transform-let* (bindings x) (body x) env) env))
   ((    begin? x) (emit-begin (cdr x) env))
   ((   labels? x) '()) ; (labels ([fname code] ...] expr)
   ((labelcall? x) '()) ; (labelcall lvar expr ...)
   ((  funcall? x) (emit-funcall (cadr x) (cddr x) env)) ; (funcall expr ...)
   ((  closure? x) (emit-closure (cadr x) (cddr x) env)) ; (closure lvar var ...)
   )
  )

;; Compile program

(define (compile-program expr)
  (emit-prolog)
  (if (labels? expr) (emit-labels expr) (emit-code "scheme_entry" '() '()  expr '()))
  (emit-epilog)
  )

(define (emit-argspc count_a count_b)
  (cond
   ((zero? count_a) "")
   ((eqv? 1 count_a) (format #f "i64 %arg~a" count_b)) 
   ( else (string-append (format #f "i64 %arg~a, " count_b) (emit-argspc (- count_a 1) (+ count_b 1))))
   ))

(define (emit-blank)
  (emit ""))

(define (emit-header fname count)
  (emit "define i64 @~a(~a)" fname (emit-argspc count 0))
  (emit "{")
  (emit "entry: ")
  (emit "%tmp = alloca i64"))

(define (emit-footer)
  (emit "%ret = load i64, i64* %tmp")
  (emit "ret i64 %ret")
  (emit "}"))

(define (emit-prolog)
  (emit "@hptr = external global i8*, align 8")
  (emit-blank))


(define (emit-epilog)
  (emit-blank)
  (emit "declare i64  @hptr_con(i64, i64)             #1")
  (emit "declare void @hptr_inc(i64)                  #1")
  (emit "declare i64  @hptr_ptr(i64)                  #1")
  (emit "declare i64  @hptr_car(i64)                  #1")
  (emit "declare i64  @hptr_cdr(i64)                  #1")
  (emit "declare i64  @hptr_closure_len()             #1")
  (emit "declare i64  @hptr_closure_lab()             #1")
  (emit "declare i64  @hptr_get_freevar(i64, i64*)    #1")
  (emit "declare void @hptr_set_clsptr(i64)           #1")
  (emit "declare i64  @hptr_get_clsptr()              #1")
  (emit "declare void @hptr_vector_set(i64, i64, i64) #1")
  (emit "declare void @hptr_vector_ref(i64, i64, i64*)#1")
  (emit "declare i64  @hptr_vector_mak(i64, i64)      #1")
  (emit "declare i64  @hptr_string_mak(i64, i64)      #1")
  (emit "declare void @stack_pop()                    #1")
  (emit "declare void @stack_psh(i64)                 #1")
  )

;; fixnum - last two bits 0, mask 11b
;; charac - taggeed with 00001111 (8 bits)
;; boolns - tagged with 0011111 (7 bits)
;; emplst - 00101111b


;; Heap Allocation

;; Tags

;; 001 - Pairs
;; 010 - Closures
;; 011 - Symbols
;; 101 - Vectors
;; 110 - Strings


;; The aim right now is to implement the datastructures Pairs, Vectors and Strings


;; In addition to the primitives required to construct them and access their data


;; The Pair and its primitives pair? [x], car[x], cdr[x] and cons[x]

(define (cons-primcall-emitter env arg1 arg2)
  (let ((label1 (get-label)) (label2 (get-label))
	(label3 (get-label)) (label4 (get-label)))
    (emit-expr arg1 env)
    (emit "%~a = load i64, i64* %tmp" label1)
    (emit-expr arg2 env)
    (emit "%~a = load i64, i64* %tmp" label2)
    (emit "%~a = call i64 @hptr_con(i64 %~a, i64 %~a)" label3 label1 label2)
    (emit "store i64 %~a, i64* %tmp" label3)
    ))

(define (car-primcall-emitter env arg)
  (let ((label1 (get-label)) (label2 (get-label)))
    (emit-expr arg env)
    (emit "%~a = load i64, i64* %tmp" label1)
    (emit "%~a = call i64 @hptr_car(i64 %~a)" label2 label1)
    (emit "store i64 %~a, i64* %tmp"  label2)))

(define (cdr-primcall-emitter env arg)
  (let ((label1 (get-label)) (label2 (get-label)))
    (emit-expr arg env)
    (emit "%~a = load i64, i64* %tmp" label1)
    (emit "%~a = call i64 @hptr_cdr(i64 %~a)" label2 label1)
    (emit "store i64 %~a, i64* %tmp"  label2)))

;; The Vector and its primitives vector?, vector, vector-set, vector-ref

;; Create Vector of Length arg1, Populated wtih arg2
(define (make-vector-primcall-emitter env arg1 arg2)
  (let ((label1 (get-label)) (label2 (get-label)) (label3 (get-label)))
    (emit-expr arg1 env)
    (emit "%~a = load i64, i64* %tmp" label1)
    (emit-expr arg2 env)
    (emit "%~a = load i64, i64* %tmp" label2)
    (emit "%~a = call i64 @hptr_vector_mak(i64 %~a, i64 %~a)" label3 label1 label2)
    (emit "store i64 %~a, i64* %tmp"  label3)
    )
  )

;; Create Vector of Length arg1, Populated wtih arg2
(define (vector-set-primcall-emitter env arg1 arg2 arg3)
  (let ((label1 (get-label)) (label2 (get-label)) (label3 (get-label)) (label4 (get-label)))
    (emit-expr arg1 env)
    (emit "%~a = load i64, i64* %tmp" label1)
    (emit-expr arg2 env)
    (emit "%~a = load i64, i64* %tmp" label2)
    (emit-expr arg3 env)
    (emit "%~a = load i64, i64* %tmp" label3)
    (emit "call void @hptr_vector_set(i64 %~a, i64 %~a, i64 %~a)" label1 label2 label3)
    )
  )

;; Create Vector of Length arg1, Populated wtih arg2
(define (vector-ref-primcall-emitter env arg1 arg2)
  (let ((label1 (get-label)) (label2 (get-label)))
    (emit-expr arg1 env)
    (emit "%~a = load i64, i64* %tmp" label1)
    (emit-expr arg2 env)
    (emit "%~a = load i64, i64* %tmp" label2)
    (emit "call void @hptr_vector_ref(i64 %~a, i64 %~a, i64* %tmp)" label1 label2)
    )
  )

;; The String and its primitives string?, make-string

(define (make-string-primcall-emitter env arg1 arg2)
  (let ((label1 (get-label)) (label2 (get-label)) (label3 (get-label)))
    (emit-expr arg1 env)
    (emit "%~a = load i64, i64* %tmp" label1)
    (emit-expr arg2 env)
    (emit "%~a = load i64, i64* %tmp" label2)
    (emit "%~a = call i64 @hptr_string_mak(i64 %~a, i64 %~a)" label3 label1 label2)
    (emit "store i64 %~a, i64* %tmp"  label3)
    )
  )


















