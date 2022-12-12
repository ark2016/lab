(load "unit-test.scm")

;; Аркадий Лебедев, Михаил Тверитнев
;; program ::= (( define | if | func | op) program) | exit | null
;; define ::= scan-func -> dict
;; func ::= op func
;; op ::= / + - * ... func
;; if ::= func | .


(define stack '())

(define-syntax safe-cons ;; cons с гарантированным порядком исполнения
  (syntax-rules()
    ((safe-cons head tail)
     (let*
         ((gl head)
          (hv tail))
       (cons gl hv))))) 

;; Стековые операции
(define (drop)
  (set! stack (cdr stack)))
(define (swap)
  (set! stack (safe-cons (cadr stack) (safe-cons (car stack) (cddr stack)))))
(define (dup)
  (set! stack (safe-cons (car stack) stack)))
(define (over)
  (set! stack (safe-cons (cadr stack) stack)))
(define (rot)
  (set! stack (safe-cons (caddr stack) (safe-cons (cadr stack) (safe-cons (car stack) (cdddr stack))))))
(define (depth)
  (set! stack (safe-cons (length stack) stack)))
(define (new-el el)
  (set! stack (safe-cons el stack)))

;; Арифметические операции
(define (sum)
  (set! stack (safe-cons (+ (car stack) (cadr stack)) (cddr stack))))
(define (sub)
  (set! stack (safe-cons (- (cadr stack) (car stack)) (cddr stack))))
(define (mul)
  (set! stack (safe-cons (* (car stack) (cadr stack)) (cddr stack))))
(define (my-div)
  (set! stack (safe-cons (/ (cadr stack) (car stack)) (cddr stack))))
(define (my-mod)
  (set! stack (safe-cons (modulo (cadr stack) (car stack)) (cddr stack))))
(define (my-neg)
  (set-car! stack (- (car stack))))

;; Операции сравнения
(define (my-eq)
  (if (= (car stack) (cadr stack))
      (begin (drop) (drop) (new-el -1))
      (begin (drop) (drop) (new-el 0))))
(define (my-less)
  (if (< (cadr stack) (car stack))
      (begin (drop) (drop) (new-el -1))
      (begin (drop) (drop) (new-el 0))))
(define (my-more)
  (if (> (cadr stack) (car stack))
      (begin (drop) (drop) (new-el -1))
      (begin (drop) (drop) (new-el 0))))

;; Логические операции
(define (my-not)
  (if (not (= (car stack) 0))
      (set-car! stack 0)
      (set-car! stack -1)))

(define (my-or)
  (if (not (= (car stack) 0)) 
      (set! stack (safe-cons (car stack) (cddr stack)))
      (if (not (= (cadr stack) 0))
          (set! stack (safe-cons (cadr stack) (cddr stack)))
          (set! stack (safe-cons 0 (cddr stack))))))
          
(define (my-and)
  (if (not (= (car stack) 0)) 
      (if (not (= (cadr stack) 0))
          (set! stack (safe-cons (cadr stack) (cddr stack)))
          (set! stack (safe-cons 0 (cddr stack))))
      (set! stack (safe-cons 0 (cddr stack)))))


;; Блоки интерпретатора
(define (scan-func fnlist)
  (define (define-frag fn) ;; проматываем define
    (if (equal? (car fn) 'end)
        '()
        (safe-cons (car fn) (define-frag (cdr fn)))))
  (define (func-cont fn) ;; продолжение 
    (if (equal? (car fn) 'end)
        (cdr fn)
        (func-cont (cdr fn))))
  (safe-cons (list (cadr fnlist) (define-frag (cddr fnlist))) (func-cont fnlist)))

(define (scan-if fnlist dict)
  (define (if-frag fn) ;; проматываем if
    (if (equal? (car fn) 'endif)
        '()
        (safe-cons (car fn) (if-frag (cdr fn)))))
  (define (func-cont fn) ;; продолжение
    (if (equal? (car fn) 'endif)
        (cdr fn)
        (func-cont (cdr fn))))
  (and (eval-func (if (and (equal? (car stack) -1) (drop)) ;; выражение if больше не нужно
                       (if-frag (cdr fnlist))
                       (and (drop) '())) dict) (func-cont fnlist)))
  
(define (eval-func fnlist dict)
  (or (null? fnlist)
      (and (equal? (car fnlist) 'if) (eval-func (let ((in-if  (scan-if fnlist dict)))
                                                   (if in-if
                                                       in-if
                                                       '())) dict))
      (and (eval-op (car fnlist) dict) (eval-func (cdr fnlist) dict))))

(define (eval-op fnel dict) ;; хороший eval
  (or (null? fnel)
      ;; почему не ассоциативный массив?
      ;; Во-первых, ключевых слов не так много, и такой массив не даёт большого выигрыша в длине кода.
      ;; Во-вторых, нахождение элемента в ассоциативном списке - весьма дорогая операция.
      ;; Замена ассоциативного массива на такое прямое решение позволила ускорить работу интерпретатора и уменьшить нагрузку на сборщик мусора.
      
      ;; арифметические операции
      (and (number? fnel) (new-el  fnel))
      (and (equal? fnel '+) (sum))
      (and (equal? fnel '-) (sub))
      (and (equal? fnel '*) (mul))
      (and (equal? fnel '/) (my-div))
      (and (equal? fnel 'mod) (my-mod))
      (and (equal? fnel 'neg) (my-neg))
      ;; стековые операции
      (and (equal? fnel 'dup) (dup))
      (and (equal? fnel 'drop) (drop))
      (and (equal? fnel 'swap) (swap))
      (and (equal? fnel 'over) (over))
      (and (equal? fnel 'rot) (rot))
      (and (equal? fnel 'depth) (depth))
      ;; операции сравнения
      (and (equal? fnel '=) (my-eq))
      (and (equal? fnel '<) (my-less))
      (and (equal? fnel '>) (my-more))
      ;; логические операции
      (and (equal? fnel 'not) (my-not))
      (and (equal? fnel 'and) (my-and))
      (and (equal? fnel 'or) (my-or))
      ;; функции из словаря
      (let ((getdict (assq fnel dict)))
        (if getdict
            (eval-func (cadr getdict) dict)
            #f))))

(define (interpret vect stack-state)
  (define (loop list dict)
    (or (null? list) ;; понятно
        (equal? (car list) 'exit) ;; тоже очевидно
        (and (equal? (car list) 'define) (let ((res (scan-func list)))
                                           (loop (cdr res) (safe-cons (car res) dict))))
        ;; добавляем обработанное определение в словарь
        (and (equal? (car list) 'if) (loop (scan-if list dict) dict))
        ;; if - он и в Африке if
        (and (eval-op (car list) dict) (loop (cdr list) dict)))) ;; операции и функции из словаря обрабатываются как обычно
  
  
  (begin (set! stack stack-state)
         (loop (vector->list vect) '())
         stack))



      
(define set-of-tests
  (list
   (test (interpret #(   define =0? dup 0 = end
                          define <0? dup 0 < end
                          define signum
                          =0? if exit endif
                          <0? if drop -1 exit endif
                          drop
                          1
                          end
                          -9 signum
                          0 signum
                          10 signum
                          ) (quote ())) '(1 0 -1))
   (test (interpret #( define =0? dup 0 = end
                        define <0? dup 0 < end
                        define -- 1 - end
                        define pow
                        =0? if drop drop exit endif
                        rot
                        over
                        *
                        rot
                        --
                        pow
                        end
                        1
                        2
                        10
                        pow) '()) '(1024))
   (test (interpret #(   define abs
                          dup 0 <
                          if neg endif
                          end
                          9 abs
                          -9 abs      ) (quote ())) '(9 9))
   (test (interpret #(   define -- 1 - end
                          5 -- --      ) '()) '(3))
   (test (interpret #(   define -- 1 - end
                          define =0? dup 0 = end
                          define =1? dup 1 = end
                          define factorial
                          =0? if drop 1 exit endif
                          =1? if drop 1 exit endif
                          dup --
                          factorial
                          *
                          end
                          0 factorial
                          1 factorial
                          2 factorial
                          3 factorial
                          4 factorial     ) (quote ())) '(24 6 2 1 1))
   (test (interpret #(   define =0? dup 0 = end
                          define =1? dup 1 = end
                          define -- 1 - end
                          define fib
                          =0? if drop 0 exit endif
                          =1? if drop 1 exit endif
                          -- dup
                          -- fib
                          swap fib
                          +
                          end
                          define make-fib
                          dup 0 < if drop exit endif
                          dup fib
                          swap --
                          make-fib
                          end
                          10 make-fib     ) (quote ())) '(0 1 1 2 3 5 8 13 21 34 55))
   (test (interpret #(   define =0? dup 0 = end
                          define gcd
                          =0? if drop exit endif
                          swap over mod
                          gcd
                          end
                          90 99 gcd
                          234 8100 gcd    ) '()) '(18 9))))

(run-tests set-of-tests)
  
      
  

