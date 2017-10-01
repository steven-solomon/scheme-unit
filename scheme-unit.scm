(define (assert value)
  (eq? value #t))

(unless (eq? (assert #t) #t) (error "asserting true should pass"))

(unless (eq? (assert #f) #f)
  (error "asserting false should fail"))

(define (execute test)
  (test))

(define (execute-all tests)
  (map execute tests))

(define (all-passing tests)
  (filter
    (lambda (result) (assert result)) tests))

(define (all-failing tests)
  (filter 
    (lambda (result) (not (assert result))) tests)) 

(define run
  (lambda (tests)
    (define results (execute-all tests))
    (define passing (all-passing results))
    (define failing (all-failing results))
    (define status 
        (if (zero? (length failing))
          "SUCCESS" "FAILING"))
    (list (length passing) (length failing) status)))

(define (failing-count report)
    (car (cdr report)))

(define (passing-count report)
    (car report))

(unless (zero? (passing-count (run '())))
  (error "empty suite should have no passes"))

(unless (zero? (failing-count (run '()))) 
  (error "empty suite should have no failures"))

(define (passing-test) (assert #t))

(define (status report)
    (car (cdr (cdr report))))

(unless (eq? (passing-count (run (list passing-test))) 1)
  (error "one passing suite should have passing"))

(unless (zero? (failing-count (run (list passing-test))))
  (error "one passing suite should have no failures"))

(unless (eq? (status (run (list passing-test))) "SUCCESS")
  (error "status should be success"))

(define (failing-test) (assert #f))

(unless (zero? (passing-count (run (list failing-test))))
  (error "failing suite should have no passing"))

(unless (eq? (failing-count (run (list failing-test))) 1)
  (error "one failing suite should have one failure"))

(unless (eq? (status (run (list failing-test))) "FAILING")
  (error "status should be failing"))

(unless (eq? (passing-count (run (list passing-test failing-test))) 1)
  (error "suite of two should have one passing"))

(unless (eq? (failing-count (run (list passing-test failing-test))) 1)
  (error "suite of two should have one failure"))

(unless (eq? (status (run (list passing-test failing-test))) "FAILING")
  (error "status should be failing"))

(display "SUCCESS")
