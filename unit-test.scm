(define-syntax test
  (syntax-rules ()
    ((test expr res)
     (lambda () (make-test 'expr
                           (lambda () expr)
                           res)))))
;; структура теста: (<цитата теста> <лямбда выражение> <ожидаемый результат>)

(define (make-test expr val expected)
  (write expr)
  (let* ((result (val))
         (ok? (equal? result expected)))
    (cond (ok? (display " ok"))
          (else
           (display " FAIL")
           (newline)
           (display "  expected: ")
           (write expected)
           (newline)
           (display "  returned: ")
           (write result)))
    (newline)
    ok?))

(define (run-test test) ;; высвобождаем make-test
  (test))

  
(define (run-tests test-list) 
  (define and-overhaul
    (lambda x ;; Проблема: нельзя взять и просто применить and ко всем элементам
      (if (null? x)
          #t
          (if (car x)
              (apply and-overhaul (cdr x))
              #f))))
  (apply and-overhaul (map run-test test-list)))


  
(define div-tests
  (list (test (/ 20 5) 4)
        (test (/ 20 0) 2222)
        (test (/ 30 4) 15/2)))