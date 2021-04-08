import sys
sys.path.append('../')
from babybel import *

# Church Numerals

# ;;; zero

cn_zero = f'''(define cz
	  (fn (z)
	    (fn (s)
	      z)))'''

# ;;; successor


cn_succ = f'''
(define cs
  (fn (cn)
    (fn (z)
      (fn (s)
        ((cn (s z)) s)))))'''

# ;;; from (non-negative) integer to Church numeral

int_to_cn = f'''
(define n2cn
  (fn (n)
    (if (= n 0)
        cz
        (cs (n2cn (- n 1))))))'''


# ;;; from Church numeral to integer 

cn_to_int = f'''
(define cn2n
  (fn (cn)
    ((cn 0) (fn (n)
              (+ n 1)))))'''

# ;;; > (cn2n (cs (n2cn 3)))
# ;;; 4
# ;;; > 

# ;;; Church numeral addition

cn_add = f'''
(define cadd
  (fn (cn1)
    (fn (cn2)
      ((cn1 cn2) cs))))'''

# ;;; > (cn2n ((cadd (n2cn 3)) (n2cn 4)))
# ;;; 7
# ;;; > 

# ;;; Church numeral multiplication

cn_mul = f'''
(define cmul
  (fn (cn1)
    (fn (cn2)
      ((cn1 cz) (cadd cn2)))))'''

# ;;; > (cn2n ((cmul (n2cn 3)) (n2cn 4)))
# ;;; 12
# ;;; > 

# ;;; Church numeral exponentiation

cn_exp = f'''
(define cexp
  (fn (cn1)
    (fn (cn2)
      ((cn2 (cs cz)) (cmul cn1)))))'''

# ;;; > (cn2n ((cexp (n2cn 2)) (n2cn 10)))
# ;;; 1024
# ;;; > 

# ;;;;;;;;;;

# Testing Recursion

# ;;; simple recursion:

identity_nat_alt = f'''(define identity_nat
  (fn (n)
    (if (= 0 n)
        0
        (+ (identity_nat (- n 1)) 1))))'''

accumulate_aux = f'''
(define accumulate_aux
  (fn (n h)
    (if (= n 0)
        (h 0)
        (accumulate_aux (- n 1) (fn (i) (h (+ i 1)))))))'''

accumulate = f'''
(define accumulate
  (fn (n)
    (accumulate_aux n (fn (i) i))))'''

# ;;; evaluating the following expression should yield 0:

# (accumulate 0)

# ;;; evaluating the following expression should yield 1:

# (accumulate 1)

# ;;; evaluating the following expression should yield 2:

# (accumulate 2)

accumulate_alt_aux = f'''
(define accumulate_alt_aux
  (fn (n h)
    (if (= n 0)
        (h 0)
        (accumulate_alt_aux (- n 1) (fn (i) (+ (h i) 1))))))'''

accumulate_alt = f'''
(define accumulate_alt
  (fn (n)
    (accumulate_alt_aux n (fn (i) i))))'''

# ;;; evaluating the following expression should yield 0:

# (accumulate_alt 0)

# ;;; evaluating the following expression should yield 1:

# (accumulate_alt 1)

# ;;; evaluating the following expression should yield 2:

# (accumulate_alt 2)

add1 = f'''
(define add1
  (fn (n)
    (+ n 1)))'''

# (add1 0)    ;;; should evaluate to 1

# (add1 (add1 0))    ;;; should evaluate to 2

# (add1 (add1 (add1 0)))    ;;; should evaluate to 3

identity_nat = f'''
(define identity_nat
  (fn (n)
    (if (= n 0)
        0
        (+ 1 (identity_nat (- n 1))))))'''

# ;;;;;

# ;;; evaluating the following expression should yield 0:

id_test_zero = '(identity_nat 0)'

# ;;; evaluating the following expression should yield true:

id_test_zero_zero = '(= (identity_nat 0) 0)'

# ;;;;;

# ;;; evaluating the following expression should yield 1:

id_test_one = '(identity_nat 1)'

# ;;; evaluating the following expression should yield true:

id_test_one_one = '(= (identity_nat 1) 1)'

# ;;;;;

# ;;; evaluating the following expression should yield 2:

id_test_two = '(identity_nat 2)'

# ;;; evaluating the following expression should yield true:

id_test_two_two = '(= (identity_nat 2) 2)'

# ;;;;;

# ;;; evaluating the following expression should yield 3:

id_test_three = '(identity_nat 3)'

# ;;; evaluating the following expression should yield true:

id_test_three_three = '(= (identity_nat 3) 3)'

# -----


identity_nat = f'''
(define identity_nat
  (fn (n)
    (if (= n 0)
        0
        (+ 1 (identity_nat (- n 1))))))'''

test_if = f'''
(if (= 2 0)
	0
	(+ 0 3))'''

test(identity_nat)
test(test_if)
test(identity_nat_alt)
test(id_test_zero) # 0
test(id_test_zero_zero) # t
test(id_test_one) # 1
test(id_test_one_one) # t
test(id_test_two) # 2
test(id_test_two_two) # t
test(id_test_three) # 3
test(id_test_three_three) # t
test(accumulate_aux)
test(accumulate)
test("(accumulate 0)")
test("(accumulate 1)")
test("(accumulate 2)")
test("(accumulate 3)")
test("(accumulate 5)")

test(accumulate_alt_aux)
test(accumulate_alt)
test("(accumulate_alt 0)")
test("(accumulate_alt 1)")
test("(accumulate_alt 2)")
test("(accumulate_alt 3)")
test("(accumulate_alt 4)")

test("(define f0 (fn (h) (h)))")
test("(define f1 (fn (n) (f0 (fn () n))))")
test("(f1 10)")

test("(define g0 (fn (n) (fn () n)))")
test("(define g1 (fn (n) ((g0 n))))")
test("(g1 10)")

test("(define h (fn (n1) (fn (n2) (fn (n3) (fn (n4) (+ n1 (+ n2 (+ n3 (+ n4 0)))))))))")
test("(define h1 (h 1))")
test("(define h2 (h1 10))")
test("(define h3 (h2 100))")
test("(h3 1000))")

test(add1)
test("(add1 0)")
test("(add1 (add1 0))")
test("(add1 (add1 (add1 0)))")

test(cn_zero)
test("cz")
test(cn_succ)
test("cs")
test(int_to_cn)
test(cn_to_int)
test(cn_add)
test(cn_mul)
test(cn_exp)

test("n2cn")
test("cn2n")
test("cadd")
test("cmul")
test("cexp")

test("(n2cn 3)") # nil
test("(= 0 0)") # t 
test("(if (= 2 0) 5 20)") # 20
test("(if (= 55 55) 5 20)") # 5
test("(cn2n (cs (n2cn 3)))") # 4
test("(cn2n ((cadd (n2cn 3)) (n2cn 4)))") # Church addition: 7
test("(cn2n ((cmul (n2cn 3)) (n2cn 4)))") # Church multiplication: 12
test("(cn2n ((cexp (n2cn 2)) (n2cn 10)))") # Church exponentiation: 1024

test("(define list (fn xs xs))")

test("list")
test("(list)")
test("(list 1)")
test("(list 1 2)")
test("(list 1 2 3)")

test("+")
test("(+ 5 10)")
test("(apply + (list 1 10))")
test("(apply apply (list + (list 1 10)))")
test("(apply (fn (x) x) (list 1))")
test("(apply (fn (x y) (list y x)) (list 1 10))")

test("((fn () (apply (fn (x) x) (list 1))))")
test("((fn () (apply + (list 1 10))))")
test("((fn () (apply apply (list + (list 21 10)))))")
test("((fn () (apply apply (list apply (list + (list 21))))))")

fix = f'''(define fix
  (fn (f)
    ((fn (x)
       (fn vs
         (apply (f (x x)) vs)))
     (fn (x)
       (fn vs
         (apply (f (x x)) vs))))))'''

test(fix)

rev_alt = f'''(define rev_alt
  (fn (vs)
    ((fix (fn (rev_acc)
             (fn (vs a)
               (if (pair? vs)
                   (rev_acc (cdr vs) (cons (car vs) a))
                   a))))
     vs '())))'''

summatorial_linear_alt = f'''(define summatorial_linear_alt
  (fix (fn (summatorial_linear)
          (fn (n)
            (if (= n 0)
                0
                (+ (summatorial_linear (- n 1)) n))))))'''

test(rev_alt)

test("(rev_alt (list 1 2 3))")

test(summatorial_linear_alt)

test("(summatorial_linear_alt 4)") # 6

fix1 = f'''(define fix1
  (fn (f)
    ((fn (x)
       (fn (v1)
         ((f (x x)) v1)))
     (fn (x)
       (fn (v1)
         ((f (x x)) v1))))))'''

test(fix1)

g_env = Env()
l_env = Env()
d_env = Env()