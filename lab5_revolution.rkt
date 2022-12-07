;;(define stack '())

(define (incr x)
  (+ x 1))

(define-syntax safe-cons
  (syntax-rules()
    ((safe-cons head tail)
     (let*
         ((gl head)
          (hv tail))
       (cons gl hv)))))

(define (to-word word lst index)
  (if (equal? (car lst) word)
      index
      (to-word word (cdr lst) (incr index))))

(define (trim lst wcount)
  (if (= wcount 0)
      lst
      (trim (cdr lst) (- wcount 1))))

(define (getbeg lst wcount zagot)
  (if (= wcount 0)
      zagot
      (getbeg (cdr lst) (- wcount 1) (safe-cons (car lst) zagot))))
  

;; Управление стеком
(define (list-drop lst)
  (and (> (length lst) 0) (cdr lst)))
(define (list-swap lst)
  (and (> (length lst) 1) (safe-cons (cadr lst) (safe-cons (car lst) (cddr lst)))))
(define (list-dup lst)
  (and (> (length lst) 0) (safe-cons (car lst) lst)))
(define (list-over lst)
  (and (> (length lst) 1) (safe-cons (cadr lst) lst)))
(define (list-rot lst)
  (and (> (length lst) 2) (safe-cons (caddr lst) (safe-cons (cadr lst) (safe-cons (car lst) (cdddr lst))))))
(define (list-depth lst)
  (safe-cons (length lst) lst))

(define (interpret program stack); списки лучше подходят для продвижения по интерпретируемой программе
  (do-all (vector-length program) (vector->list program) 0 stack '() '()))

(define (do-all max-length now-program word-counter stack-data stack-return dict)
  (if (= max-length word-counter)
      (begin  (display now-program) (display stack-return) (display stack-data)  (display dict) (display "\n"))
      (let ((symbol (car now-program))
            (stack-rem (cdr now-program)))   
        (cond
          ((equal? symbol 'exit) (display stack-data))
          ;; Арифметические операции
          ((number? symbol)
           (do-all max-length stack-rem (incr word-counter) (safe-cons (car now-program) stack-data) stack-return dict))
          ((equal? symbol '+)
           (do-all max-length stack-rem (incr word-counter)
                   (safe-cons (+ (cadr stack-data) (car stack-data)) (cddr stack-data)) stack-return dict))
          ((equal? symbol '-)
           (do-all max-length stack-rem (incr word-counter)
                   (safe-cons (- (cadr stack-data) (car stack-data)) (cddr stack-data)) stack-return dict))
          ((equal? symbol '*)
           (do-all max-length stack-rem (incr word-counter)
                   (safe-cons (* (cadr stack-data) (car stack-data)) (cddr stack-data)) stack-return dict))
          ((equal? symbol '/)
           (do-all max-length stack-rem (incr word-counter)
                   (safe-cons (/ (cadr stack-data) (car stack-data)) (cddr stack-data)) stack-return dict))
          ((equal? symbol 'mod)
           (do-all max-length stack-rem (incr word-counter)
                   (safe-cons (modulo (cadr stack-data) (car stack-data)) (cddr stack-data)) stack-return dict))
          ((equal? symbol 'neg)
           (do-all max-length stack-rem (incr word-counter)
                   (safe-cons (- (car stack-data)) (cdr stack-data)) stack-return dict))
          ;; Операции сравнения, true = -1, false = 0
          ((equal? symbol '=)
           (do-all max-length stack-rem (incr word-counter)
                   (safe-cons (if (= (car stack-data) (cadr stack-data)) 0 -1) (cddr stack-data))  stack-return dict))
          ((equal? symbol '>)
           (do-all max-length stack-rem (incr word-counter)
                   (safe-cons (if (> (car stack-data) (cadr stack-data)) 0 -1) (cddr stack-data))  stack-return dict))
          ((equal? symbol '<)
           (do-all max-length stack-rem (incr word-counter)
                   (safe-cons (if (< (car stack-data) (cadr stack-data)) 0 -1) (cddr stack-data))  stack-return dict))
          ;; Логические операции
          ((equal? symbol 'not)
           (do-all max-length stack-rem (incr word-counter)
                   (safe-cons (if (= (car stack-data) 0) -1 0) (cddr stack-data))  stack-return dict))
          ((equal? symbol 'and)
           (do-all max-length stack-rem (incr word-counter)
                   (safe-cons (if (and (= (car stack-data) -1) (= (cadr stack-data) -1)) -1 0) (cddr stack-data))  stack-return dict))
          ((equal? symbol 'or)
           (do-all max-length stack-rem (incr word-counter)
                   (safe-cons (if (or (= (car stack-data) -1) (= (cadr stack-data) -1)) -1 0) (cddr stack-data))  stack-return dict))
          ;; Операции со стеком
          ((equal? symbol 'drop)
           (do-all max-length stack-rem (incr word-counter) (list-drop stack-data)  stack-return dict))
          ((equal? symbol 'swap)
           (do-all max-length stack-rem (incr word-counter) (list-swap stack-data)  stack-return dict))
          ((equal? symbol 'dup)
           (do-all max-length stack-rem (incr word-counter) (list-dup stack-data)  stack-return dict))
          ((equal? symbol 'over)
           (do-all max-length stack-rem (incr word-counter) (list-over stack-data)  stack-return dict))
          ((equal? symbol 'rot)
           (do-all max-length stack-rem (incr word-counter) (list-rot stack-data)  stack-return dict))
          ((equal? symbol 'depth)
           (do-all max-length stack-rem (incr word-counter) (list-depth stack-data) stack-return dict))
          ;; Оставшиеся ключевые слова
          ((equal? symbol 'define)
           (let* ((deflen (to-word 'end stack-rem word-counter))
                  (to-cut (- deflen word-counter))
                  (prog-cont (trim (cdr stack-rem) to-cut))
                  (prog-piece (reverse (getbeg stack-rem to-cut '()))))
             (display prog-cont) (display prog-piece) (display "\n")
             (do-all (length prog-cont) prog-cont 0
                     stack-data stack-return (safe-cons (list (car prog-piece) (cdr prog-piece)) dict))))
          
          ((equal? symbol 'end)
           (do-all max-length stack-rem (incr word-counter) stack-data stack-return dict))
          
          ((equal? symbol 'if)
           (if (not (= (car stack-data) 0))
               (do-all max-length stack-rem (incr word-counter) (cdr stack-data) stack-return dict)
               (let* ((iflen (to-word 'endif stack-rem word-counter))
                      (to-cut (- iflen word-counter))
                      (prog-cont (trim (cdr stack-rem) to-cut)))
             (do-all max-length prog-cont  iflen (cdr stack-data) stack-return dict))))
           ;;(do-all max-length prog-cont (incr iflen) (cdr stack-data) stack-return dict))))
          
          ((assoc symbol dict)
           (do-all max-length stack-rem (incr word-counter) (append stack-data (cadr (assoc symbol dict))) stack-return dict))
          (else (display (cadr dict)))))))
          
              
;; ((equal? word 'if) (interpreter (if (zero? (car stack)) (inc (find-word 'endif program index)) (inc index)) (cdr stack) return-stack definitions))
         
          
         
          
