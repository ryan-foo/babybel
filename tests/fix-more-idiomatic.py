import sys
sys.path.append('../src/')
from eval import *
from reader import *
from eval_test import *

'''
;;; fix-more-idiomatic.bel

;;; One fix to pull them all, 
;;; One fix to mind them,
;;; One fix to code them all,
;;; And in our programs, be them.
'''

fix = f'''(define fix
  (fn (f)
    ((fn (x)
       (fn vs
         (apply (f (x x)) vs)))
     (fn (x)
       (fn vs
         (apply (f (x x)) vs))))))'''

'''
;;;;;;;;;;
'''

fib = f'''(define fib
  (fn (n)
    (if (= n 0) 0
        (= n 1) 1
        (+ (fib (- n 1)) (fib (- n 2))))))'''


test_fib = f'''(define test_fib
  (fn (candidate n)
    (= (candidate n) (fib n))))'''

'''
;;;;;;;;;;
'''

fib_alt = f'''(define fib_alt
  (fix (fn (fib)
         (fn (n)
           (if (= n 0) 0
               (= n 1) 1
               (+ (fib (- n 1)) (fib (- n 2))))))))'''


'''
;;; evaluating (test_fib fib_alt 5) yields true

;;;;;;;;;;
'''

test(fix)
test(fib)
test(test_fib)
test("(fib 10)")
test("(test_fib fib 5)")
# test(fib_alt) # doesn't work because we don't have apply, or fix.
# test("(test_fib fib_alt 5)")

fib_lin_alt = f'''(define fib_lin_alt
  (fn (n)
    ((fix (fn (fibfib)
            (fn (n a b)
              (if (= n 0) a
                  (fibfib (- n 1) b (+ a b)))))) n 0 1)))'''

# ;;; evaluating (test_fib fib_lin_alt 5) yields true

fib_cps_alt = f'''(define fib_cps_alt
  (fn (n)
    ((fix (fn (fib_cps_aux)
            (fn (n k)
              (if (= n 0) (k 0)
                  (= n 1) (k 1)
                  (fib_cps_aux (- n 1) (fn (n1)
                                         (fib_cps_aux (- n 2)
                                                      (fn (n2)
                                                        (k (+ n1 n2))))))))))) n (fn (a) a)))'''

# ;;; evaluating (test_fib fib_lin_alt 5) yields true

# test(fib_lin_alt)
# test("(test_fib fib_lin_alt 5)")

# test(fib_cps_alt)
# test("(test_fib fib_cps_alt 5)")

# ;;;;;;;;;;

# ;;; end of fix-more-idiomatic.bel