import sys
sys.path.append('../src/')
from eval import *
from reader import *

# Init

test_env = False
test_basics = False
test_prim = False
test_eval = True

# Defining Axioms

bel_nil = Symbol("nil")
bel_t = Symbol("t")
bel_o = Symbol("o")
bel_apply = Symbol("apply")
bel_lit = Symbol("lit")
bel_prim = Symbol("prim")
bel_clo = Symbol("clo")

# Testing pairs made out of Association Lists, and basic types

nil = Symbol("nil")
one = Number(1)
five = Number(5)
ten = Number(10)

if test_basics:
	print(atomp(ten))
	print(Symbol("nil"))
	print(bel_nil)
	print(atomp(bel_nil)) # Yes, it is a symbol.›
	test_list1 = make_list(one, five, ten)
	print(test_list1)
	print(test_list1.length())
	test_list2 = make_list(one)
	print(test_list2)
	print(test_list2.length())

# Testing Env and env lookup

one_sym = Symbol("one")
one = Number(1)
two_sym = Symbol("two")
two = Number(2)

if test_env:
	'''
	TODO: Handle printing env bug later.
	print(l_env)
	'''
	print("Init env tests")
	l_env.push(one_sym, one)
	l_env.push(two_sym, two)
	print(env_lookup(l_env, Symbol("one")))
	print(env_lookup(l_env, one_sym))
	print(l_env)
	print(g_env)
	print(d_env)


# Testing primitives

if test_prim:
	hello = make_string("hello")
	print("Make a string out of hello: %s" % hello)
	print("Is hello a Bel string?: %s" % stringp(hello))
	print("Result of converting Bel string to python string: %s" % bel_to_python_string(hello))
	print("Type of result: %s" % type(bel_to_python_string(hello)))


# Testing Eval

if_tests = ["(if (> 5 2) 'true 'false)", "(if (< 5 2) 'true 'false)", "(if (< 5 2) 'true)", "(if (not (< 5 2)) 'true)"]
fn_tests = ["(fn (x) (+ x 1))",
	"(lit clo nil (x) (+ x 1))", "((fn (x) (+ x 1)) 3)", 
	"(((fn (x) (fn (y) (+ x y))) 1) 2)", 
	"((fn (f) (f (f 1))) (fn (x) (+ x 1)))",
	"((fn (succ) ((fn (f) (f (f 1))) (fn (x) (succ x)))) (fn (x) (+ x 1)))", 
	"((fn (succ) ((fn (f) (f (f 1))) succ)) (fn (x) (+ x 1)))",
	"(define x 1) x",
	"(let ((x 1)) x)",
	"(let ((x 1) (y 2)) (+ x y))",
	"x",
	"1",
	"(let ((x 1)) (f x))",
	"(let ((x 1) (y 2)) (+ x y))",
	"(set x 20)",
	"(set y 50 z 100)",
	"(set n (lit clo nil p e))",
	"(def n p e)",
	"(set x 'a)",
	"(id 5 5)"]

read = g.get_sexpr
gt_true_test = g.get_sexpr(if_tests[0])
lt_false_test = g.get_sexpr(if_tests[1])
fn_test_1 = g.get_sexpr(fn_tests[0]) # + x 1
fn_test_2 = g.get_sexpr(fn_tests[1])
fn_test_3 = g.get_sexpr(fn_tests[2])
fn_test_4 = g.get_sexpr(fn_tests[3])
fn_test_5 = g.get_sexpr(fn_tests[4])
fn_test_6 = g.get_sexpr(fn_tests[5])
fn_test_7 = g.get_sexpr(fn_tests[6]) # Successor Function
fn_test_8 = g.get_sexpr(fn_tests[7]) # Define Function
fn_test_9 = g.get_sexpr(fn_tests[8]) # Let Function
fn_test_10 = g.get_sexpr(fn_tests[9]) # Let, scope
fn_test_11 = g.get_sexpr(fn_tests[10]) # Eval x
fn_test_12 = g.get_sexpr(fn_tests[11]) # Eval 1
fn_test_13 = g.get_sexpr(fn_tests[12]) # let... result = ((fn (x) (f x)) 1)
fn_test_14 = g.get_sexpr(fn_tests[13]) # (let ((x 1) (y 2)) (+ x y)) = ((fn (x y) (+ x y)) 1 2)
fn_test_15 = g.get_sexpr(fn_tests[14]) # set x 20
fn_test_16 = g.get_sexpr(fn_tests[15]) # set y 50 z 100
fn_test_17 = g.get_sexpr(fn_tests[16]) # set n (lit clo nil p e)
fn_test_18 = g.get_sexpr(fn_tests[17]) # (def n p e) should eval to the above 
fn_test_19 = g.get_sexpr(fn_tests[18]) # return a
fn_test_20 = g.get_sexpr(fn_tests[19]) # return a


def test_list(n):
	test = g.get_sexpr(fn_tests[n])
	print("test %s: %s" % (n, test))
	print("result of eval on test %s: %s" % (n, eval(test, bel_nil)))

def expr_test(expr):
	test = g.get_sexpr(expr)
	print("test: %s" % test)
	print("result of eval on test: %s" % eval(test, bel_nil))

# Now you can evaluate something, in a lexical environment where successor is defined.

if test_eval:
	# print(eval(Number(1), g_env)) # Evaluating a number 1 returns 1
	# print(eval(bel_nil, g_env)) # Evaluating a symbol returns itself
	# print(type(eval(bel_nil, g_env))) # And we check that it's a symbol.
	# print(eval(bel_t, g_env)) # returns t.
	# print(eval(one_sym, l_env)) # The symbol one is unbound.
	# print(gt_true_test) # gt true test
	# print(type(gt_true_test.a)) 
	# print(gt_true_test.a) # the symbol if
	# hello_string = make_string("hello") # Hello.
	# print(type(hello_string.a)) # It's a char! But we should have a separate printing for. How do you know if something is a string beforehand, before evaluating the entire thing? What if its a pair with a char in one side and a function to be evaluated in another? I get that strings are literals, but this is all too much -- perhaps something simple like "print_string" should be enough.
	# print(make_string("hello")) # ("hello"), but it's (h e l l o) which is also okay I suppose
	# print(fn_test_1)
	# print(fn_test_1.a)
	# print(type(fn_test_1.a))
	# print(fn_test_1.d)
	# print(eval(fn_test_1, l_env))
	# print(fn_test_2)
	# print(eval(fn_test_2, l_env)) # This will not evaluate properly because we have disabled the literalp branch of eval.
	# Why does + still eval to nil?
	# print(g_env)
	# print(g_env)
	# itr = g_env.d
	# print(itr)
	# cand = itr.a
	# print(cand)
	# print(cand.a)
	# print(Symbol("+").n)
	# print(idp(Symbol("+"),Symbol("+")))

	# print(symbol_table)
	# print(symbol_table.keys())
	# [print(type(a)) for a in symbol_table.keys()]
	# test_list(2)
	# test_list(6)
	# test_list(7) # Define dont work because it evaluates x and it becomes nil. So define can never push 1 as the variable for the symbol table. We could make it a special form and have it directly access if symbol is correct, but not sure?
	# test_list(10)
	# test_list(11)
	# test_list(12)
	# test_list(13)
	# test_list(0)
	# test_list(1)
	# test_list(14)
	# # test_list(15)
	# test_list(16)
	# # test_list(17)
	# # test_list(18)
	# test_list(10)
	# test_list(19)
	# expr_test("n")
# fn_tests = ["(fn (x) (+ x 1))", 
	# "(lit clo nil (x) (+ x 1))", "((fn (x) (+ x 1)) 3)"


	# Test by printing eval, which expressions we use, later

	# Debug so that eval reaches function application again


	# print((read("(+ x 1)").a, l_env))
	# print(lookup(l_env, Symbol("+")))
	# print(eval(Symbol("+"), l_env))
	# print(gt_true_test)
	# print(eval(gt_true_test, l_env))
	# print(lt_false_test)
	# print(eval(lt_false_test, l_env))

	'''
	Several expressions in one string.
	Read expression by expression.
	'''

	'''
	Errors are happening because addition, subtraction etc aren't bound to the functions on the symbol table. I.E Looking up the symbol + doesn't point to the addition function, so there's no function to be applied. We will try to apply nil, (since that is what is returned) but nil isn't a function, so we throw an error.

	Proposed Solution: We need to instantiate a function, do_add, do_sub etc that represents the relevant addition function (in the case of (+ x 1).).

	We have tried to do this by adding the new functions via setup_environment (you can see - is associated with <function Number.__sub__>, which is its Python representation.). We also disabled the guard rails on Pair (i.e, we stop checking if its a Bel Type) and same for builtin method push for the Environment.

	But still, env_lookup isn't returning the number add function when we call it with g_env and the Symbol ("+"). So it may be a problem with env_lookup?

	We go further and investigate, turns out that lookup is not looking up properly in the global environment. It is either a problem with lookup or our environment.

	I checked, it seems that == is too strict (and Symbol("+") is not equal to Symbol("+")! So we try idp now and it works, i.e it returns true. However it still says no when we try to look up Symbol("+") in g_env, even though Symbol+ is already bound in global environment to function Number.__add__.)

	Our error came when environment answers YES to being an atom. Because our new class Env (which no longer inherits from Pair) says they're an atom, we just return nil. aghh!

	The thing is, we cannot say it's a pair because then the environment will always expect to be a pair. It is a list, and its first element is bel_nil, sure. But we keep trying to access the car of Env, which doesnt exist!

	Basically: look at lookup_env, class Env.. etc
	'''

	# print(eval(read("(+ x 1)"), read("((x . 3) (nil) . nil)")))

	'''
	I studied a lot about the culture of Lisp, and came out with this knowledge...
	Present a short synthesis of that --
	it will have intellectual content.

	New Jersey vs MIT School
	CLisp vs Scheme
	- architectural choices...
	- The Cathedral and the Bazaar

	https://www.google.com/search?q=the+cathedral+and+the+bazaar&rlz=1C5CHFA_enSG889SG889&oq=the+cathedral+and+the+bazaar&aqs=chrome..69i57j46j0l8.2712j0j1&sourceid=chrome&ie=UTF-8

	Involution Ocean & John McCarthy
	- Pragmatic, who wanted to do things with Lisp, not just Lisp...

	'''

	'''
	How we solve the bug of environments:
	we have an environment class, that you have to access with e.
	We have to modify all our other environment interacting functions to be
	very respectful of the environment.

	So bind, for example, now uses push to extend the environment.
	And binding any new params and args will look at env.e, not just extending
	the pair of the lexical environment.

	So now what needs to be modified is how we use the new output from bind.
	Let's try using env.push onto the result of bind.
	'''


'''
Bug: Two things are both nil on the symbol table. Our identity predicate for symbols checks if looking up x on the symbol table is the same as looking up y on the symbol table. So when we eval, it goes to the symbol table, looks them both up, sees that they're both "nil" and then evaluating fn goes into the quote branch, and tries to call quote.

We need to fix idp, and the way it compares. Either that or, instantiate the symbols fn etc with proper non-nil values.

Fixed: x.n == y.n, not look up on the symbol table

Evaluating fn_test_1 correctly:
first it goes to the pairp branch. It matches car with the symbol fn.
It prints "We are making a function."
It calls make_closure, with the lexical environment and the cdr ((x) (+ x 1))
This calls make_literal with the keyword clo (closure), and the actual parameters, and the body.
'''


'''
Changes since then

I've added sym func map

mapping + to the Number.__add__ function which is defined as a Python function

question: how to do Bel functions for addition etc?

It goes through, it manages to successfully map the function from lit clo nil etc...

and extend the environment (x. 3)

then look up the function in the global environment - and get a Python function back (Number.__add__)
and pass it the correct args (3 1)

but now I run into an error trying to apply that function because it's not defined in Bel!

So how would I define an addition function as a Bel class? / type?
'''

# def setup_environment(g_env, sym_func_map):
#     '''
#     Takes a list of tuples, of symbols to funcs. Maps those symbols to the functions in the initial environment. i.e addition becomes __add__, and so on.
#     '''
#     for i in range(len(sym_func_map)):
#         g_env.push(Symbol(sym_func_map[i][0]), sym_func_map[i][1])
#     return g_env


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

def test(test_string):
	test = g.get_sexpr(test_string)
	print("test: %s" % (test))
	result = eval_tp(test, bel_nil)
	print("result of eval on test: %s \n" % (result))
	return result