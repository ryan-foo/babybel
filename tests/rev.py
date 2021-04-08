import sys
sys.path.append('../')
from babybel import *

'''
;;; rev.bel
;;; the list-reverse function

;;;;;;;;;;

;;; standard tail-recursive definition with an accumulator
'''

rev_acc = f'''(define rev_acc
  (fn (vs a)
    (if (pair? vs)
        (rev_acc (cdr vs) (cons (car vs) a))
        a)))'''

rev = f'''(define rev
  (fn (vs)
    (rev_acc vs '())))'''

test(rev_acc)
test(rev)
listp = f'''(define list (fn xs xs))'''
test(listp)
# test("(list)")
# test("(list 1)")
# test("(list 1 2 3)")
# print(make_list(1, 2, 3, 4, 5))

# test("(car (cdr (cdr (cons 1 (cons 2 (cons 3 nil))))))")
# test("(list 1 2 3)")
# test("(car (list 1 2 3))")
test("(rev (cons 1 (cons 2 (cons 3 nil))))")
# test("(rev (list 1 2 3))")


# test("(rev '())")
# test("pair?")
# test("(pair? 2)")
# test("(pair? nil)")
# test("(pair? (cons 1 2))")
# test("(if (pair? '(1 2)) 1 2)")
# test("(rev (cons 1 nil))")
# test("(rev '(1))")
# test("(cdr (cons 1 2))")
# test("(rev (cons 1 (cons 2 nil)))")
# test("(rev (cons 1 (cons 2 (cons 3 nil))))")
# test("(pair? '())")
# test("(pair? 1)")


# list1 = (make_list(Number(1), Number(2), Number(3)))
# print(proper_listp(list1))

# test("(cons 1 (cons 2 (cons 3 nil)))")

'''
;;;;;;;;;;

;;; definition using a fixed-point combinator (a.k.a. a Y combinator)
# '''

fix2 = f'''(define fix2
  (fn (f)
    ((fn (x)
       (fn (v1 v2)
         ((f (x x)) v1 v2)))
     (fn (x)
       (fn (v1 v2)
         ((f (x x)) v1 v2))))))'''

rev_alt = f'''(define rev_alt
  (fn (vs)
    ((fix2 (fn (rev_acc)
             (fn (vs a)
               (if (pair? vs)
                   (rev_acc (cdr vs) (cons (car vs) a))
                   a))))
     vs '())))'''

# '''
# ;;;;;;;;;;

# ;;; (tail-recursive) generation of a list of n successive non-negative integers
# '''

# iota_aux = f'''(define iota_aux
#   (fn (n a)
#     (if (= n 0)
#         a
#         (iota_aux (- n 1) (cons (- n 1) a)))))'''

# iota = f'''(define iota
#   (fn (n)
#     (iota_aux n '())))'''

# test(rev_acc)
# test(rev)
# test(fix2)
# test(rev_alt)
# test(iota_aux)
# test(iota)

# test("(iota 10)")
# # test("cons")
# # print(type(test("(cons 1 2)")))
# '''
# ;;; evaluating (iota 10) yields (0 1 2 3 4 5 6 7 8 9)

# ;;;;;;;;;;

# ;;; simple unit test: reversing the reverse of a list yields this list
# '''

# '''
# EQUAL
# '''

# nullp = f'''(define null?
#   (fn (v)
#     (id v nil)))'''

# equalp = f'''(define equal?
#   (fn (v1 v2)
#     (if (id v1 v2)
#         t
#         (if (pair? v1)
#             (if (pair? v2)
#                 (if (null? (cdr v1))
#                     (if (null? (cdr v2))
#                         (equal? (car v1) (car v2))
#                         nil)
#                     (if (equal? (car v1) (car v2))
#                         (equal? (cdr v1) (cdr v2))
#                         nil))
#                 nil)
#             (if (pair? v2)
#                 nil
#                 nil)))))'''

# test(nullp)
# test(equalp)

# test("(equal? '(5) '(5))")

# test_rev = f'''(define test_rev
#   (fn (candidate n)
#     ((fn (vs)
#        (equal? (candidate (candidate vs)) vs))
#      (iota n))))'''

# test(test_rev)
# test("(test_rev rev 10)")
# # test("equal?")
# # test("(equal? 5 2)")
# # test("(equal? 5 2)")
# # test("(id 5 2)")
# test("(equal? '(1 2 3) '(1 2 3))")

# # test("(null?)")

# # test("(car (cons 1 2))")
# # test("(cons 0 (cons 1 2))")
# # test("(car (cons 0 (cons 1 2)))")
# # test("(car (cons 0 1))")
# # test("(define y (cons 1 2))")
# # test("(define z (cons 1 2))")
# # test("(equal? 5 5)")
# # test("(equal? y z)")
# # test("(type z)")

# # one_two = make_list(1, 2)
# # two_three = make_list(2, 3)


# # ;;; > (test_rev rev 10)
# # ;;; #t
# # ;;; > (test_rev rev_alt 10)
# # ;;; #t
# # ;;; > 

# '''
# ;;;;;;;;;;

# ;;; end of rev.bel
# '''