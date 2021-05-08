import sys
sys.path.append('../src/')
from eval import *
from reader import *
from eval_test import *

'''
;;; fib-more-idiomatic.bel
;;; the Fibonacci function with a more idiomatic use of if-expressions

;;;;;;;;;;

;;; standard recursive definition
'''

fib = f'''(define fib
  (fn (n)
    (if (= n 0) 0
        (= n 1) 1
	(+ (fib (- n 1)) (fib (- n 2))))))'''

'''
;;;;;

;;; simple unit-test function
'''

test_fib = f'''(define test_fib
  (fn (candidate n)
    (= (candidate n) (fib n))))'''

'''
;;;;;;;;;;

;;; iterative definition that works in linear time
'''

fibfib = f'''(define fibfib
  (fn (n a b)
    (if (= n 0)
        a
        (fibfib (- n 1) b (+ a b)))))'''

fib_lin = f'''(define fib_lin
  (fn (n)
    (fibfib n 0 1)))'''

# ;;; evaluating (test_fib fib_lin 5) yields true

test(fib)
test(test_fib)
test(fibfib)
test(fib_lin)

'''
;;;;;;;;;;

;;; standard recursive definition in CPS
'''

fib_cps = f'''(define fib_cps
  (fn (n k)
    (if (= n 0) (k 0)
        (= n 1) (k 1)
        (fib_cps (- n 1) (fn (n1)
                           (fib_cps (- n 2)
                                    (fn (n2)
                                      (k (+ n1 n2)))))))))'''

fib_alt = f'''(define fib_alt
  (fn (n)
    (fib_cps n (fn (a) a))))'''

test(fib_cps)
test(fib_alt)
test("(fib 5)")
test("(test_fib fib 5)")
test("(test_fib fib_lin 5)")
test("(test_fib fib_alt 5)")

'''
;;; evaluating (test_fib fib_cps 5) yields true

;;;;;;;;;;

;;; three Y-combinators: one for functions of arity 1, one for functions of arity 2, and one for functions of arity 3
'''

fix1 = f'''(define fix1
  (fn (f)
    ((fn (x)
       (fn (v1)
         ((f (x x)) v1)))
     (fn (x)
       (fn (v1)
         ((f (x x)) v1))))))'''

fix2 = f'''(define fix2
  (fn (f)
    ((fn (x)
       (fn (v1 v2)
         ((f (x x)) v1 v2)))
     (fn (x)
       (fn (v1 v2)
         ((f (x x)) v1 v2))))))'''

fix3 = f'''(define fix3
  (fn (f)
    ((fn (x)
       (fn (v1 v2 v3)
         ((f (x x)) v1 v2 v3)))
     (fn (x)
       (fn (v1 v2 v3)
         ((f (x x)) v1 v2 v3))))))'''

'''
;;;;;;;;;;

;;; re-definition of fib using fix1
'''

fib_alt = f'''(define fib_alt
  (fix1 (fn (fib)
          (fn (n)
            (if (= n 0) 0
                (= n 1) 1
                (+ (fib (- n 1)) (fib (- n 2))))))))'''

# ;;; evaluating (test_fib fib_alt 5) yields true

test(fix1)
test(fix2)
test(fix3)
test(fib_alt)

test("(test_fib fib_alt 5)") # t

'''
;;;;;;;;;;

;;; re-definition of fib_lin using fix3
'''

fib_lin_alt = f'''(define fib_lin_alt
  (fn (n)
    ((fix3 (fn (fibfib)
             (fn (n a b)
               (if (= n 0)
                   a
                   (fibfib (- n 1) b (+ a b)))))) n 0 1)))'''

# ;;; evaluating (test_fib fib_lin_alt 5) yields true

test(fib_lin_alt)
print("BEGINNING")
test("(test_fib fib_lin_alt 5)")
# test("(- 5 1)")
# test("(g_env)")
# test("(+ 0 1)")
# test("(fib_lin_alt 12)")
# test("(if 1)")
# test("((fn () (if nil 1 (+ 2 0))))")
# test("(if nil 1 nil 3 nil)")

'''
;;;;;;;;;;

;;; re-definition of fib_cps using fix2
'''

fib_cps_alt = f'''(define fib_cps_alt
  (fn (n)
    ((fix2 (fn (fib_cps_aux)
             (fn (n k)
               (if (= n 0) (k 0)
                   (= n 1) (k 1)
                   (fib_cps_aux (- n 1) (fn (n1)
                                          (fib_cps_aux (- n 2)
                                                       (fn (n2)
                                                         (k (+ n1 n2)))))))))) n (fn (a) a))))'''

test(fib_cps_alt)
test("(fib_cps_alt 6)")
# test("(test_fib fib_cps_alt 5)")

# test("(define x 1)")
# test("x")

# ;;; evaluating (test_fib fib_lin_alt 5) yields true

# test(fib_alt)
# test("(test_fib fib_alt 5)")
# test("test_fib fib_lin_alt 5)")

'''
;;;;;;;;;;

;;; end of fib-more-idiomatic.bel
'''
