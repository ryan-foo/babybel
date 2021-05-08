# church_test.py

import sys
sys.path.append('../src/')
from eval import *
from eval_test import *

# Church Numerals

# ;;; zero

cn_zero = f'''(define 'cz
	  (fn (z)
	    (fn (s)
	      z)))'''

# ;;; successor


cn_succ = f'''
(define 'cs
  (fn (cn)
    (fn (z)
      (fn (s)
        ((cn (s z)) s)))))'''

# ;;; from (non-negative) integer to Church numeral

int_to_cn = f'''
(define 'n2cn
  (fn (n)
    (if (= n 0)
        cz
        (cs (n2cn (- n 1))))))'''


# ;;; from Church numeral to integer 

cn_to_int = f'''
(define 'cn2n
  (fn (cn)
    ((cn 0) (fn (n)
              (+ n 1)))))'''

# ;;; > (cn2n (cs (n2cn 3)))
# ;;; 4
# ;;; > 

# ;;; Church numeral addition

cn_add = f'''
(define 'cadd
  (fn (cn1)
    (fn (cn2)
      ((cn1 cn2) cs))))'''

# ;;; > (cn2n ((cadd (n2cn 3)) (n2cn 4)))
# ;;; 7
# ;;; > 

# ;;; Church numeral multiplication

cn_mul = f'''
(define 'cmul
  (fn (cn1)
    (fn (cn2)
      ((cn1 cz) (cadd cn2)))))'''

# ;;; > (cn2n ((cmul (n2cn 3)) (n2cn 4)))
# ;;; 12
# ;;; > 

# ;;; Church numeral exponentiation

cn_exp = f'''
(define 'cexp
  (fn (cn1)
    (fn (cn2)
      ((cn2 (cs cz)) (cmul cn1)))))'''

# ;;; > (cn2n ((cexp (n2cn 2)) (n2cn 10)))
# ;;; 1024
# ;;; > 

# ;;;;;;;;;;

def test(test_string):
	test = g.get_sexpr(test_string)
	print("test: %s" % (test))
	print("result of eval on test: %s" % (eval(test, bel_nil)))

test("5")
# test(cn_zero)
# test("cz")
# test(cn_succ)
# test("cs")
# test(int_to_cn)
# test("n2cn")
# test(cn_to_int)
# test("cn2n")
# test("(cn2n (cs (n2cn 3)))")
