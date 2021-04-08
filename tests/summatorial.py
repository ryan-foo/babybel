import sys
sys.path.append('../')
from babybel import *

'''
;;; summatorial.bel
;;; summing the first given n natural numbers

;;;;;;;;;;
'''

summatorial_Gauss = f'''(define summatorial_Gauss
  (fn (n)
    (/ (* n (+ n 1)) 2)))'''

'''
;;;;;;;;;;

;;; a simple unit-test function
'''

test_summatorial = f'''(define test_summatorial
  (fn (candidate n)
    (= (candidate n) (summatorial_Gauss n))))'''

'''
;;;;;;;;;;

;;; a recursive implementation
'''

summatorial_linear = f'''(define summatorial_linear
  (fn (n)
    (if (= n 0)
        0
        (+ (summatorial_linear (- n 1)) n))))'''

# ;;; evaluating (test_summatorial summatorial_linear 20) yields true

test(summatorial_Gauss)
test(test_summatorial)
test(summatorial_linear)

test("(test_summatorial summatorial_linear 20)") # t

'''
;;;;;;;;;;

;;; a tail-recursive implementation with an accumulator
'''

summatorial_linear_acc_aux = f'''(define summatorial_linear_acc_aux
  (fn (n a)
    (if (= n 0)
        a
        (summatorial_linear_acc_aux (- n 1) (+ n a)))))'''

summatorial_linear_acc = f'''(define summatorial_linear_acc
  (fn (n)
    (summatorial_linear_acc_aux n 0)))'''

# ;;; evaluating (test_summatorial summatorial_linear_acc 20) yields true

test(summatorial_linear_acc_aux)
test(summatorial_linear_acc)

test("(test_summatorial summatorial_linear_acc 20)") # t

'''
;;;;;;;;;;

;;; two Y-combinators (i.e., fixed-point operators): one for unary functions, and one for binary functions
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

test(fix1)
test(fix2)

'''
;;;;;;;;;;

;;; the recursive summatorial function defined with a Y combinator:
'''

summatorial_linear_alt = f'''(define summatorial_linear_alt
  (fix1 (fn (summatorial_linear)
          (fn (n)
            (if (= n 0)
                0
                (+ (summatorial_linear (- n 1)) n))))))'''

# ;;; evaluating (test_summatorial summatorial_linear_alt 20) yields true

test(summatorial_linear_alt)
test("(test_summatorial summatorial_linear_alt 20)") # t

'''
;;;;;;;;;;

;;; the tail-recursive summatorial function defined with a Y combinator:
'''

summatorial_linear_acc_alt = f'''(define summatorial_linear_acc_alt
  (fn (n)
    ((fix2 (fn (summatorial_linear_acc_aux)
             (fn (n a)
               (if (= n 0)
                   a
                   (summatorial_linear_acc_aux (- n 1) (+ n a)))))) n 0)))'''

# ;;; evaluating (test_summatorial summatorial_linear_acc_alt 20) yields true

test(summatorial_linear_acc_alt)
test('(test_summatorial summatorial_linear_acc_alt 20)') # t

'''
;;;;;;;;;;

;;; end of summatorial.bel
'''
