''' 
beginning of babybel.py
8th Apr 2021
A tail-recursive interpreter for Babybel, written in Python

Ryan Foo
Supervised by Professor Olivier Danvy
'''

'''
SECTIONS:
(0) Misc
(1) Reader
(2) Interpreter
(3) REPL
'''

'''
MISC

Useful constants, et al.
'''

from enum import Enum
import random
import sys
import string
import types
import traceback
import math

debug = True
SYMBOLCANDIDATES = string.digits + string.ascii_letters + '-+*/:&$?='
CHARCANDIDATES = ['\\' + char for char in SYMBOLCANDIDATES]

WS = string.whitespace
SPECIAL = "()`',@"
SYNTAX = WS + SPECIAL

'''
READER
'''

'''
The job of the parser is to extract an abstract syntax tree,
which we can then pass to eval.

Adapted from William Annis's PyLisp reader, 
on his website (biostat.wisc.edu).
'''

class Reader:
    def __init__(self, str=None):
        self.str = str
        self.i = 0
        self.len = 0
        self.sexpr = []
        self.pounds = {}

        if str:
            self.sexpr = self.get_sexpr()

    def add_pound_helper(self, char, helper):
        self.pounds[char] = helper

    def get_token(self):
        if self.i >= self.len:
            return None

        while self.i < self.len and self.str[self.i] in WS:
            self.i = self.i + 1

        if self.i == self.len:
            return None

        # Now, tokenize.
        if self.str[self.i] == '#':
            self.i = self.i + 2
            return self.pounds[self.str[self.i - 1]](self.get_token())
        if self.str[self.i] in SPECIAL:
            self.i = self.i + 1
            return self.str[self.i - 1]
        elif self.str[self.i] == '"':
            # Parse a string.
            str = ""
            self.i = self.i + 1
            while self.str[self.i] != '"' and self.i < self.len:
                if self.str[self.i] == '\\':
                    self.i = self.i + 1
                    spchar = self.str[self.i]
                    if spchar == "n":
                        str = str + "\n"
                    elif spchar == "t":
                        str = str + "\t"
                else:
                    str = str + self.str[self.i]
                self.i = self.i + 1
            self.i = self.i + 1
            return String(str)
        else:
            tok = ""
            # First, build the token.
            while self.i < self.len - 1:
                if self.str[self.i] in SYNTAX:
                    break
                else:
                    tok = tok + self.str[self.i]
                    self.i = self.i + 1

            if not self.str[self.i] in SYNTAX:
                tok = tok + self.str[self.i]
                self.i = self.i + 1
            try:
                tok = int(tok)
            except:
                pass
            if isinstance(tok, int) or isinstance(tok, float):
                tok = Number(tok)
            else:
                tok = Symbol(tok)

            return tok

    def get_sexpr(self, str=None):
        if str:
            self.i = 0
            self.str = str
            self.len = len(self.str)
        '''
        expr: a Python list of Bel expressions.
        '''
        expr = None
        tok = self.get_token()
        if tok == ')':
            raise SyntaxError("Unexpected ')'")
        elif tok == "(":
            expr = []
            tok = self.get_token()
            while tok != ")":
                if tok == '(':
                    self.i = self.i - 1
                    expr.append(self.get_sexpr())
                elif tok == "'":
                    expr.append(make_list(Symbol("quote"), self.get_sexpr()))
                elif tok == "`":
                    expr.append(make_list(Symbol("iquote"), self.get_sexpr()))
                elif tok == None:
                    raise SyntaxError("unexpected end of expression")
                else:
                    if isinstance(tok, int) or isinstance(tok, float):
                        tok = Number(tok)
                    expr.append(tok)

                tok = self.get_token()
            return make_list(*expr)
        elif tok == "'":
            return make_list(Symbol("quote"), self.get_sexpr())
        elif tok == "`":
            return make_list(Symbol("iquote"), self.get_sexpr())
        else:
            return tok

'''
INTERPRETER
'''

chars_char = SYMBOLCANDIDATES
chars_ord = list(map(ord, str(chars_char)))
chars_bin = [format(char, '08b') for char in chars_ord]
chars = dict(zip(chars_char, chars_bin))
# This will be included in the global env later.


'''
Defining Bel Types as Python Classes
'''

'''
Every type is implemented in Python as a BelType object.
They inherit from BelType, and are either a Char, Pair, Symbol, Stream or Number.
Numbers are non-standard but implementing them makes our job easier.
'''

class BelType():

    '''
    Abstract class for others to inherit from. The base class of every type in Bel.
    '''
    def __init__(self):
        pass
    def isbeltype(self):
        return True

    def beltype(self):
        if isinstance(self, Symbol):
            return "symbol"
        if isinstance(self, Char):
            return "char"
        if isinstance(self, String):
            return "pair"
        if isinstance(self, Pair):
            return "pair"
        else:
            raise TypeError("This is not a Bel type.")

    def __repr__(self):
        '''
        This will be extended by the other types. (Pairs, for example.)
        '''
        if nilp(self):
            return "()"
        elif atomp(self):
            return ("%s") % (self)
        elif pairp(self):
            return "(%s)" % print_pair_aux(self.a, self.d)
        else:
            raise TypeError("Not a Bel Type.")

'''
PRINTING AND READING
'''

def print_val(v):
    if nilp(v):
        return "()"
    elif atomp(v):
        return print_atom(v)
    elif pairp(v):
        return print_pair(v)
    else:
        raise TypeError("Not a Bel Type.")

class Atom(BelType):
    '''
    Abstract class to inherit from. Atoms are always true, unless if they're nil.
    Everything in Bel is an Atom, except Pairs.
    '''

    def __init__(self):
        pass

    def istrue(self, name):
        if self.n == "nil":
            return False
        else:
            return True

class Symbol(Atom):
    def __init__(self, name):
        if [char in SYMBOLCANDIDATES for char in name]:
        # When we instantiate a symbol, every char within that symbol should be a valid symbol.
            self.n = name # variable
            self.v = None # value
            if name not in symbol_table: #Update symbol table if its not there.
                symbol_table[self.n] = self.v

    def __repr__(self):
        return self.n

    def __len__(self):
        return 1

    def cons(self, item):
        '''
        Stick yourself on the back of an item, and that becomes a pair.
        '''
        return Pair(item, self)

    def get(self, property):
        try:
            return symbol_table[self.n][property]
        except:
            return "nil"

    def istrue(self, name):
        if self.n == "nil":
            return False
        else:
            return True

class Char(Atom):
    def __init__(self, name):
        if __name__ in CHARCANDIDATES:
            self.n = name

    def __repr__(self):
        return self.n

    def cons(self, item):
        return Pair(item, self)    

'''
AXIOM SYMBOLS
'''

symbol_table = {}

bel_nil = Symbol("nil")
bel_t = Symbol("t")
bel_o = Symbol("o")
bel_apply = Symbol("apply")
bel_lit = Symbol("lit")
bel_prim = Symbol("prim")
bel_clo = Symbol("clo")

symbol_table["nil"] = Symbol("nil")
symbol_table["t"] = Symbol("t")
symbol_table["o"] = Symbol("o")
symbol_table["apply"] = Symbol("apply")
symbol_table["chars"] = chars

class Number(Atom):
    def __init__(self, value):
        if isinstance(value, int) or isinstance(value, float):
            self.v = value
        else:
            raise TypeError("You need to use a float or int to construct a number.")

    def __len__(self):
        return 1

    def length(self):
        return 1

    def cons(self, item):
        return Pair(item, self)

    def __repr__(self):
        return str(self.v)

    def __gt__(self, other):
        if type(self) == (type(other)):
            if self.v > other.v:
                return 1
            else:
                return 0
        else:
            return -1

    def __cmp__(self, other):
        if type(self) == type(other):
            if self.v == other.v:
                return 0
            elif self.v > other.v:
                return 1
            else:
                return -1

    def __add__(self, other):
        if type(self) == type(other):
            return Number(self.v + other.v)
        else:
            return self.v + other
    __radd__ = __add__

    def __sub__(self, other):
        if type(self) == type(other):
            return Number(self.v - other.v)
        else:
            return self.v - other
    __rsub__ = __sub__

    def __mul__(self, other):
        if type(self) == type(other):
            return Number(self.v * other.v)
        else:
            return self.v * other
    __rmul__ = __mul__

    def __truediv__(self, other):
        if type(self) == type(other):
            return Number(self.v / other.v)
        else:
            return self.v / other.v
    __rtruediv__ = __truediv__

class Pair(BelType):
    def __init__(self, a = bel_nil, d = bel_nil):
        self.a = a
        self.d = d

    def __repr__(self):
        return "(%s)" % print_pair_aux(self.a, self.d)

    def replacea(self, val):
        if val.isbeltype():
            self.a = val
            return val
        else:
            raise TypeError("%s is not a Beltype" % (val))

    def replaced(self, val):
        if val.isbeltype():
            self.d = val
            return val
        else:
            raise TypeError("%s is not a Beltype" % (val))

    def car(self):
        return self.a

    def cdr(self):
        return self.d

    def cons(self, item):
        return Pair(item, self)

    def length(self):
        '''
        Return length.
        Traverse the list.
        If it's not a proper list, break.
        '''
        if not proper_listp(self):
            raise TypeError("Not a proper list")
        leng = 0
        itr = self
        while not nilp(itr):
            leng += 1
            itr = itr.d
        return leng

def print_pair_aux(a, d):
    if nilp(d):
        return a
    elif atomp(d):
        return ("%s . %s") % (a, d)
    else:
        return ("%s %s") % (a, print_pair_aux(d.a, d.d))

class String(BelType):
    '''
    A string is a list, where all the args are chars.
    '''
    def __init__(self, string):
        self.str = make_string(string)

class Stream(BelType):
    def __init__(self, status):
        status = status
        raise NameError("Unimplemented")

'''
Predicates
Thanks to luksamuk's Believe
'''

def symbolp(x: BelType):
    return isinstance(x, Symbol)

def nilp(x: BelType):
    return symbolp(x) and x.n == "nil"

def pairp(x: BelType):
    return isinstance(x, Pair)

def atomp(x: BelType):
    return not (pairp(x))

def charp(x: BelType):
    return isinstance(x, Char)

def streamp(x: BelType):
    return isinstance(x, Stream)

def numberp(x: BelType):
    return isinstance(x, Number)

def idp(x: BelType, y: BelType):
    '''
    If they are different types,
    then return false.

    Otherwise, check their value,
    or check their space in memory (is).
    '''
    if (x.isbeltype() and y.isbeltype()) and (type(x) == type(y)):
        if symbolp(x):
            return x.n == y.n
        elif charp(x):
            return x.n[chars] == y.n[chars]
        elif numberp(x):
            return x.v == y.v
        else:
            return (x is y)
    else:
        if (x.isbeltype() and y.isbeltype()):
            return False
        else:
            raise TypeError("Identity can only be called on Bel types")

def proper_listp(x: BelType):
    '''
    (1 2 3) is a proper list
    (1 . (2 . (3 . nil))) is a proper list
    (1 2 3 . 4) is not a proper list
    nil is a proper list
    () is a proper list

    We traverse the list, pairwise. If the cdr is nil, it is proper. 
    If it is a pair, it continues. If the cdr is anything else, it is not a list!
    '''
    if not pairp(x) and not nilp(x):
        return bel_nil
    itr = x
    while not nilp(itr):
        if not pairp(itr):
            return bel_nil
        itr = itr.d

    return 1

def number_listp(x: BelType):
    '''
    Both a proper list,
    and a list of numbers.
    '''
    if not pairp(x) and not nilp(x):
        return False
    itr = x
    while not nilp(itr):
        car = itr.a
        if not pairp(itr):
            return False
        if not numberp(car):
            return False
        itr = itr.d
    return True


def stringp(x: BelType):
    '''
    An object is a string if it is a proper list of characters.
    '''
    if not pairp(x) and not nilp(x):
        return False
    itr = x
    while not nilp(itr):
        car = itr.a
        if not pairp(itr):
            return False
        if not charp(car):
            return False
        itr = itr.d
    return True

def literalp(x: BelType):
    '''
    Takes a proper list. Is the list a literal -- is the first element the symbol "lit".
    '''
    if not proper_listp(x):
        return False

    return idp(x.a, Symbol("lit"))

def primitivep(x: BelType):
    '''
    Takes a literal.
    Is it a primitive?
    - Primitive: second element of the list is the symbol "prim".
    '''
    return literalp(x) and idp(x.d.a, Symbol("prim"))

def closurep(x: BelType):
    '''
    Takes a literal.
    Is it a closure?
    - Closure: second element of the list is the symbol "clo".
    '''
    return literalp(x) and idp(x.d.a, Symbol("clo"))

def quotep(x: BelType):
    '''
    Tests if a list is a quoted form.
    '''
    if not proper_listp(x):
        return 0

    return idp(x.a, Symbol("quote"))

'''
Symbol table and symbols

We implement the symbol table as a Python dictionary.
'''


'''
Pairs and functions that operate on them
'''

# Takes multiple args and constructs a Bel List. But these multiple args are NOT passed as a Python list. Esxample: make_list(1, 2, 3) -> the Bel list (1 2 3)
def make_list(*args):
    '''
    * is the unpacking operator, and will construct
    a tuple out of those args, to be passed one by one to the make_list function.
    '''

    if (len(args) <= 0):
        return bel_nil

    base_pair = Pair(args[-1], bel_nil)
    result_list = base_pair

    for i in range(len(args)-2,-1,-1):
        result_list = Pair(args[i], result_list)

    return result_list

# make_string takes a python string and transforms it into a Bel one. Utility for making string.

def make_string(string):
    n = len(string)

    if (n == 0):
        return Symbol("nil");

    # Strings are represented internally as a Bel List of characters. We will make a list of chars.
    # We use the list comprehension to transform string into a Python list of Bel chars.
    string = [Char(char) for char in string]
    # Then, we unpack the args and make a Bel list of those Bel chars.
    bel_string = make_list(*string)
    return bel_string

def bel_to_python_string(string):
    '''
    Traverses a Bel string,
    takes out the character
    values individually and
    adds them to a python string.
    '''
    res = ''

    if not stringp(string):
        raise TypeError("Not a Bel String")
        '''
        Since it is a string,
        all of its elements are chars,
        and it is a proper list.
        '''
    else:
        itr = string
        while not nilp(itr):
            res = res + itr.a.n
            itr = itr.d
    return res

class Env():
    '''
    An environment is a list of pairs, and each pair (var . val) is the binding of a symbol var to the value val.
    '''
    def __init__(self, env = bel_nil):
        '''
        The environment begins as an empty list.
        '''
        self.e = env

    def __repr__(self):
        '''
        The environment will always be a list of pairs.
        So printing an environment is either the same 
        as printing out a list, or printing out an empty
        list, aka bel_nil.
        '''
        if self.e == bel_nil:
            return "%s" % (bel_nil)
        else:
            '''
            Otherwise,
            we can treat it as any other list.
            '''
            return "(%s)" % print_pair_aux(self.e.a, self.e.d)

    def push(self, var, val):
        '''
        Add a var.val pair to the environment. 
        '''
        if (var.isbeltype()) and (val.isbeltype()):
            new_pair = Pair(var, val)
            self.e = Pair(new_pair, self.e)

        else:
            raise TypeError("Type not assigned, please instantiate var val pair with a BelType")

l_env = Env()
d_env = Env()

def env_lookup(env, symbol):
    '''
    Traverses an environment given a symbol. It returns the associated value or returns nil.
    '''
    if nilp(env):
        return (bel_nil, False)

    if nilp(env.e):
        return (bel_nil, False)

    elif atomp(env.e):
        return (bel_nil, False)

    elif not symbolp(symbol):
        raise TypeError("We cannot look up %s, as it is not a symbol" % (symbol))

    else:
        itr = env.e
        while (not nilp(itr)):
            cand = itr.a
            # The candidate pair is the first var /val pair of the iterator. If var is the symbol we are looking for, then return it's associated val. Otherwise, continue down the list.
            if (symbolp(cand.a) and idp(symbol, cand.a)):
                return (cand.d, True)
            else:
                itr = itr.d
                if isinstance(itr, Env):
                    itr = itr.e
    return (bel_nil, False)

def lookup(l_env, symbol):
    '''
    A lookup function that traverses the dynamic scope, then the lexical scope, then the global scope.

    Any variable is either bound in dynamic, lexical or global environments.

    What 

    It will serve useful to have a very fast lookup for symbols used often. The most often used symbols will be in the global environment.

    "First do it right, then do it fast."
    "Early optimization is the source of a lot of evil." - Donald Knuth

    How

    - lookup in the lexical scope and dynamic scope, we can use an association list because environments tend to be small.
    - Global environment
        - Direct access in a table (the symbol table is implemented as a hash table), so we can access those variables in constant time.
    '''

    # Dynamic scope
    value, found = env_lookup(d_env, symbol)
    if found:
        return value

    # Lexical scope
    value, found = env_lookup(l_env, symbol)
    if found:
        return value

    # Global scope
    value = symbol_table[symbol.n]
    return value

'''
Clears out the old val associated with symbol in a given environment and makes way for the new_val.
'''

def replace_val_in_env(env, symbol, new_val):
    if nilp(env):
        return bel_nil

    itr = env.e
    while not nilp(itr):
        cand = itr.a
        if idp(symbol, (cand.a)):
            cand.d = new_val
            return symbol
        itr = itr.d
    return bel_nil

'''
Assigns a symbol to a value. It tries to do so in the dynamic, then lexical, then global envs.
'''

def assign_val(l_env, symbol, new_val):
    # Dynamic assignment
    ret = replace_val_in_env(d_env, symbol.n, new_val)
    if not nilp(ret):
        return symbol

    # Lexical assignment
    ret = replace_val_in_env(l_env, symbol.n, new_val)
    if not nilp(ret):
        return symbol

    # When no assignment is made, we push a global value
    symbol_table[symbol.n] = new_val

'''
Literals 
are Bel objects that evaluate to themselves.

They are seen in the form (lit . rest), where lit is a symbol, and rest is a proper list of things.

Primitives and functions are literals.

Generating literals: it creates a pair where the car is the symbol lit, and the cdr is anything that should be treated as a literal (i.e, it should evaluate to itself.)
'''

def make_literal(rest):
    if not proper_listp(rest):
        raise TypeError("%s is not a proper list, it cannot be turned into a literal." % rest)
    return Pair(Symbol("lit"), rest)

'''
Primitives are literals.
'''

# LIST OF PRIMITIVES

def make_primitive(symbol):
    return make_literal(Pair(bel_prim, Pair(symbol, bel_nil)))

def register_primitive_in_env(x):
    '''
    x is a Python string representation of the primitive.
    This function will take our global env (symbol table) and register the primitives on it.
    '''
    symbol = Symbol(x)
    symbol_table[x] = make_primitive(symbol)

primitives = ["id", "join", "car", "cdr", "type", "xar", "xdr", "sym", "nom", "wrb", "rdb", "ops", "cls", "stat", "coin", "sys", "+", "-", "*", "/", "<", "<=", ">", ">=", "=", "err", "define", "def", "apply", "pair?", "cons", "g_env", "apply"]

def generate_primitives(primitives):
    for primitive in primitives:
        register_primitive_in_env(primitive)

generate_primitives(primitives)

def make_closure(l_env, rest):
    '''
    Creating a closure.
    A list must have two elements:
    one, a lambda list, and the second should be the body of the function.
    '''
    return make_literal(Pair(bel_clo, Pair(l_env, rest)))

'''
EVALUATION
'''

'''
eval, or the evaluation function, takes an expression, identifies what it is, and evaluates it.

When a simple application is performed, we take a list and consider the first element the symbol that the function is bound to.

We evaluate every element of the list, including the function, before applying the closure to the rest of the evaluated elements, which then will be passed as arguments to the function.

The closure captures the lexical env of when it is evaluated.
'''

def eval(exp, l_env):

    if numberp(exp):
        return exp

    elif symbolp(exp):
        # If they are an axiom symbol, evaluate to themselves.
        if (idp(exp, bel_nil) or idp(exp, bel_t) or idp(exp, bel_o)):
            return exp
        else:
        # Otherwise, look them up in the environment, from dynamic to lexical to global.
            return lookup(l_env, exp)

    elif quotep(exp):
        return special_quote(exp, l_env)

    elif literalp(exp):
        # Literals evaluate to themselves.
        return exp

    elif stringp(exp):
        # Strings evaluate to themselves.
        return exp

    elif pairp(exp):
        if idp(exp.a, Symbol("fn")):
            # Construct lit, clo, lexical environment, and the body of the function, and return the (lit clo l_env (formal parameters) (body) etc.)
            return make_closure(l_env, exp.d)

        elif idp(exp.a, Symbol("if")):
            # print("Special If")
            # We will call special_if on the cdr of the expr, since we already know that it's a pair, and that the first half of the pair is if.
            # We will eval_tp on the same expression,
            # trusting eval_tp to handle it with an iterative
            # approach.
            return eval_tp(exp, l_env)

        elif idp(exp.a, Symbol("set")):
            return special_set(exp.d, l_env)

        elif idp(exp.a, Symbol("define")):
            return special_set(exp.d, l_env)

        elif idp(exp.a, Symbol("def")):
            return special_set(exp.d, l_env)

        # Otherwise it is an application of a function.
        else:
            '''
            Anything that calls eval recursively
            will not be done in a straight recursive 
            manner.
            Instead, we call eval_tp, which evaluates
            the body of a closure. This instantiates
            a while loop.
            '''
            return function_apply(eval(exp.a, l_env),
                     eval_list(exp.d, l_env))

    raise Error("%d is not a proper list, you cannot apply a function." % exp)

'''
eval_tp is a function that ensures proper tail recursion of eval,
that is called to evaluate the body of a closure.

A Bel expression is ordinarily evaluated through recursive calls to eval,
while tail-recursive sub-expressions are evaluated through the iteration
of the while loop in eval_tp.
'''

def eval_tp(exp, l_env):
    while(True):
        if numberp(exp):
            return exp

        elif symbolp(exp):
            # If they are an axiom symbol, evaluate to themselves.
            if (idp(exp, bel_nil) or idp(exp, bel_t) or idp(exp, bel_o)):
                return exp
            else:
            # Otherwise, look them up in the environment, from dynamic to lexical to global.
                return lookup(l_env, exp)

        elif quotep(exp):
            return special_quote(exp, l_env)

        elif literalp(exp):
            # Literals evaluate to themselves.
            return exp

        elif stringp(exp):
            # Strings evaluate to themselves.
            return exp

        elif pairp(exp):
            if idp(exp.a, Symbol("fn")):
                # Construct lit, clo, lexical environment, and the body of the function, and return the (lit clo l_env (formal parameters) (body) etc.)
                return make_closure(l_env, exp.d)

            elif idp(exp.a, Symbol("if")):
                body = exp.d

                if nilp(body.d):
                    exp = body.a
                    continue

                if not pairp(body.d):
                    raise SyntaxError("The cdr of this pair has to be a pair.")

                else:
                    # If the cdr of the body is a pair, then
                    # The car of that pair is the test, and the cadr is the consequent branch.
                    test = body.a
                    conseq = body.d.a
                    # We evaluate the test, with a non-tail call to eval.
                    if not nilp(eval(test, l_env)):
                        # If the test evaluates to a truthy value (not nil), then return evaluation of the consequent.
                        exp = conseq
                        continue
                    else:
                        if nilp(body.d.d.d):
                            exp = body.d.d.a # Else branch: exp
                            continue
                        else:
                            # The if-then-else is not finished, so construct an if expression.
                            exp = Pair(Symbol("if"), body.d.d)
                            continue

            elif idp(exp.a, Symbol("set")):
                return special_set(exp.d, l_env)

            elif idp(exp.a, Symbol("define")):
                return special_set(exp.d, l_env)

            # Otherwise it is an application of a function.
            else:
                fun = eval(exp.a, l_env)
                args = eval_list(exp.d, l_env)

                if primitivep(fun):
                    '''
                    Function could be predefined (Primitives),
                    in which case:
                    they are applied to lists of Bel values
                    - the arity is checked
                    - each argument is fetched from args [a Bel List]
                    types are tested
                    the actual values are extracted from each value
                    the operation at hand is carried out on the values
                    we formulate a new result
                    '''

                    if fun.d.d.a == "apply":
                        arity_check(args, 2)
                        a_fun = args.a
                        a_args = args.d.a

                        while(True):
                            if closurep(a_fun):
                                l_env = a_fun.d.d.a # current lexical environment
                                params = a_fun.d.d.d.a # actual parameters
                                body = a_fun.d.d.d.d.a # function body (value that the fn returns)
                                new_env = Env(bind(params, a_args, l_env))

                                exp = body
                                l_env = new_env
                                break

                            elif (primitivep(a_fun) and a_fun.d.d.a != "apply"):
                                return apply_primitive(
                                    a_fun.d.d.a, a_args)

                            else:
                                arity_check(a_args, 2)
                                a_fun = a_args.a
                                a_args = a_args.d.a
                                continue

                    else:
                        return apply_primitive(fun.d.d.a, args)

                elif closurep(fun):
                    '''
                    When we apply a closure, we fetch it's first lexical environment.
                    Then we fetch the actual parameters.
                    Then we extend the new environment using params and args.
                    '''
                    l_env_prime = fun.d.d.a # current lexical environment
                    params = fun.d.d.d.a # actual parameters
                    l_env = Env(bind(params, args, l_env_prime)) # args: formal parameters.
                    # this gives us the new lexical environment to evaluate exp in.
                    exp = fun.d.d.d.d.a # expression, function body (value that the fn returns)
                    continue

                else:
                    raise TypeError("Not a function")

    raise TypeError("%d is an unidentified function object (UFO)." % exp)

def bind(params, args, l_env):
    '''
    Args is a list of values.
    If param is a symbol,
    then that is a variadic expression,
    then you extend, or cons params with args
    on top of l_env.

    Creates a new environment.
    '''
    if symbolp(params):
        return Pair(Pair(params, args), l_env)

    elif pairp(params):
        if (symbolp(params.a)):
            if pairp(args):
                '''
                Then we are in the case where params and args are a pair.
                So we can then take the car of both, and cons them.
                '''
                return Pair(Pair(params.a, args.a), bind(params.d, args.d, l_env))
            else:
                raise IndexError("Arity mismatch -- we have too little args")

        else:
            raise SyntaxError("Illegal formal -- formal needs to be a symbol")

    elif nilp(params):
        if nilp(args):
            return l_env
        else:
            raise IndexError("Arity mismatch -- we have too many args")

    else:
        raise SyntaxError("Illegal formal -- formal needs to be a pair or a symbol")

def function_apply(fun, args):
    if primitivep(fun):
        '''
        Function could be predefined (Primitives),
        in which case:
        they are applied to lists of Bel values
        - the arity is checked
        - each argument is fetched from args [a Bel List]
        types are tested
        the actual values are extracted from each value
        the operation at hand is carried out on the values
        we formulate a new result
        '''

        return apply_primitive(
            fun.d.d.a, args)

    elif closurep(fun):
        l_env = fun.d.d.a # current lexical environment
        params = fun.d.d.d.a # actual parameters
        body = fun.d.d.d.d.a # function body (value that the fn returns)
        new_env = Env(bind(params, args, l_env))

        return eval_tp(body, new_env) # evaluate the body given the new extended environment

    else:
        raise TypeError("Not a function")


def eval_list(bel_list, l_env):
    if nilp(bel_list):
        return bel_nil

    eval_head = eval(bel_list.a, l_env)

    eval_rest = eval_list(bel_list.d, l_env)

    return Pair(eval_head, eval_rest)

'''
PRIMITIVE FUNCTIONS

When you look up these symbols (id, join, '+' -- you are returned with "lit prim +" in the symbol table). The environment has been updated by generate_primitives, which registers these primitives. This leads us to the primitive branch in eval, which then tells us to call these primitives accordingly.
'''

def lookup_prim(sym, lit):
    return idp(sym, Symbol(lit))

def arity_check(args, num):
    length = args.length()
    if length > num:
        raise IndexError("Arity mismatch. Too many args.")
    if length < num:
        raise IndexError("Arity mismatch. Too little args.")
    '''
    If they are equal,
    then it passes
    and the rest of the primitive function
    continues to execute.
    If they are not,
    it raises an arity mismatch error and execution stops.
    '''

def apply_primitive(sym, args):
    '''
    You get the symbol "+", for example, from (lit prim +).
    This is used to look up the correct primitive to apply.

    Recall that args is a Bel list. (and it could be empty,
    or a list of 1.)

    You do an arity check, you fetch the (evaluated) arguments 
    to the function from the args list,
    apply the necessary depending on what the function specifies,
    and return the evaluated result in the form of a Bel value.

    Voila! Primitive applied.
    '''

    '''
    FUNCTIONS
    '''
    if lookup_prim(sym, "id"):
        return prim_id(args)
    elif lookup_prim(sym, "join"):
        return prim_join(args)
    elif lookup_prim(sym, "car"):
        return prim_car(args)
    elif lookup_prim(sym, "cdr"):
        return prim_cdr(args)    
    elif lookup_prim(sym, "type"):
        return prim_type(args)
    elif lookup_prim(sym, "xar"):
        return prim_xar(args)
    elif lookup_prim(sym, "xdr"):
        return prim_xdr(args)
    # elif lookup_prim(sym, "sym"):
    #     return prim_sym(args)
    # elif lookup_prim(sym, "nom"):
    #     return prim_nom(args)
    # elif lookup_prim(sym, "wrb"):
    #     return prim_wrb(args)
    # elif lookup_prim(sym, "rdb"):
    #     return prim_rdb(args)
    # elif lookup_prim(sym, "ops"):
    #     return prim_ops(args)
    # elif lookup_prim(sym, "cls"):
    #     return prim_cls(args)
    # elif lookup_prim(sym, "stat"):
    #     return prim_stat(args)        
    elif lookup_prim(sym, "coin"):
        return prim_coin(args)
    elif lookup_prim(sym, "sys"):
        return prim_sys(args)
    elif lookup_prim(sym, "apply"):
        return prim_apply(args)
    # Babybel Primitives
    elif lookup_prim(sym, "pair?"):
        if pairp(args.a):
            return bel_t
        else:
            return bel_nil
    elif lookup_prim(sym, "cons"):
        return prim_cons(args)
    # OPERATORS
    elif lookup_prim(sym, "+"):
        return prim_add(args)
    elif lookup_prim(sym, "-"):
        return prim_sub(args)
    elif lookup_prim(sym, "*"):
        return prim_mul(args)
    elif lookup_prim(sym, "/"):
        return prim_div(args)
    elif lookup_prim(sym, "<"):
        return prim_lt(args)
    elif lookup_prim(sym, "<="):
        return prim_leq(args)
    elif lookup_prim(sym, ">"):
        return prim_gt(args)
    elif lookup_prim(sym, ">="):
        return prim_geq(args)
    elif lookup_prim(sym, "="):
        return prim_eq(args)
    elif lookup_prim(sym, "let"):
        return prim_let(args)
   # OTHER PRIMITIVES
    elif lookup_prim(sym, "err"):
        return prim_err(args)
    elif lookup_prim(sym, "g_env"):
        return prim_genv(args)
    # Otherwise it's not a primitive.
    else:
        print(args)
        raise ValueError("Unknown Bel primitive.")

def prim_id(args):
    '''
    Identity compares two elements
    to see if they are identical.
    '''
    arity_check(args, 2)
    if idp(args.a, args.d.a):
        return bel_t
    else:
        return bel_nil

# Bel truth is not the same as Python truth...

def prim_join(args):
    '''
    Join creates a pair with x as car and y as cdr.
    '''
    arity_check(args, 2)
    return Pair(args.a, args.d.a)

def prim_car(args):
    '''
    Returns the head of the pair.
    '''
    arity_check(args, 1)
    return args.a.a

def prim_cdr(args):
    '''
    Returns the rest of the pair.
    '''
    arity_check(args, 1)
    return args.a.d

def prim_cons(args):
    '''
    Cons does the same thing as join: it creates a pair with x as car and y as cdr, for now.
    We can consider extending it to be variadic.
    '''
    arity_check(args, 2)
    return Pair(args.a, args.d.a)


def prim_type(args):
    arity_check(args, 1)
    if isinstance(args, Symbol):
        return Symbol("symbol")
    elif isinstance(args, Char):
        return Symbol("char")
    elif isinstance(args, Pair):
        return Symbol("pair")
    elif isinstance(args, Stream):
        return Symbol("stream")
    elif isinstance(args, Number):
        return Symbol("number")
    else:
        raise TypeError("Not a Bel type")

def prim_xar(args):
    arity_check(args, 2)
    pair = args.a
    val = args.d.a
    if not pairp(pair):
        raise TypeError("%s is not a pair" % pair)
    else:
        pair.a = val
    return val

def prim_xdr(args):
    arity_check(args, 2)
    pair = args.a
    val = args.d.a
    if not pairp(pair):
        raise TypeError("%s is not a pair" % pair)
    else:
        pair.d = val
    return val

def prim_sym(args):
    '''
    Takes a bel string x and gives us a symbol.
    '''
    arity_check(args, 1)
    string = args.a
    if not stringp(string):
        raise TypeError("%s is not a string." % string)
    python_string = bel_to_python_string(string)
    return Symbol(python_string)

def prim_nom(args):
    '''
    Takes a symbol x and returns a list of characters that correspond to the name of x.
    '''
    raise ValueError("Unimplemented")

def prim_apply(args):
    '''
    It takes two args.
    It has to be an applicable,
    either a primitive function,
    or a closure.

    We do two things:
    We implement apply as part of the menagerie of predefined values.
    We define apply in the while loop
    of proper tail recursion.
    Apply will preserve proper tail
    recursion.

    The first argument we can apply
    is something we can apply. (a primitive
    or a closure)

    The second argument is a Bel list.

    Apply the first argument to the second.
    '''
    arity_check(args, 2)
    a_fun = args.a
    a_args = args.d.a

    if primitivep(a_fun):    
        return apply_primitive(
            a_fun.d.d.a, a_args)

    elif closurep(a_fun):
        l_env = a_fun.d.d.a # current lexical environment
        params = a_fun.d.d.d.a # actual parameters
        body = a_fun.d.d.d.d.a # function body (value that the fn returns)
        new_env = Env(bind(params, a_args, l_env))

        return eval_tp(body, new_env) # evaluate the body given the new extended environment

    else:
        raise TypeError("application of %s in apply: Not a function" % a_fun)

'''
Functions related to streams.
'''

def prim_wrb(args):
    '''
    Tries to write a bit x to the stream y.
    '''
    arity_check(args, 2)
    raise ValueError("Unimplemented")

def prim_rdb(args):
    '''
    Tries to read a bit from the stream x.
    '''
    arity_check(args, 1)
    raise ValueError("Unimplemented")

def prim_ops(args):
    '''
    Opens a stream that writes to or reads from the place whose name is the string x,
    depending on whether y is out or in respectively.
    '''
    arity_check(args, 2)
    raise ValueError("Unimplemented")

def prim_cls(args):
    '''
    Closes stream args.a.
    '''
    arity_check(args, 1)
    raise ValueError("Unimplemented")

def prim_stat(args):
    '''
    Returns either closed, in or out depending on stream state.
    '''
    raise ValueError("Unimplemented")

'''
Others.
'''

def prim_coin(args):
    '''
    Returns either t and nil randomly.
    '''
    arity_check(args, 0)
    rand = random.randint(0, 1)
    if rand == 0:
        return bel_nil
    else:
        return bel_t

def prim_sys(args):
    '''
    Sends x (presumably a string) as a command to the os.
    '''
    raise ValueError("Unimplemented")

'''
OPERATORS

A note. Nice thing here is that since we have already done the work
of defining operators prematurely upon Numbers, shadowing the Python functions
__add__, __sub__ and the like, then we can just call them using the necessary operators, having specified exactly how they should behave.
'''

def prim_add(args):
    '''
    Variadic addition.
    Adds all values in the list. (+ 1 2 3 4) would
    evaluate to 10.

    If it is a singleton, return itself.
    '''
    if not number_listp(args):
        raise TypeError("Cannot add non-numbers.")

    length = args.length()

    if length == 0:
        return Number(0)
    elif length == 1:
        return args.a

    res = args.a
    itr = args.d

    while not nilp(itr):
        res = res + itr.a
        itr = itr.d

    return res

def prim_sub(args):
    '''
    Variadic subtraction.
    Takes a list of numbers.

    Subtracts all values in the list. (- 1 2 3 4) would
    evaluate to -10.

    If it is a singleton, then invert the value of the single number.
    '''

    # Broken on variadic numbers. 

    if not number_listp(args):
        raise TypeError("Cannot subtract non-numbers.")

    length = args.length()

    if length == 0:
        return Number(0)

    elif length == 1:
        return prim_mul(Number(-1), args.a)

    res = args.a
    itr = args.d

    while not nilp(itr):
        res = res - itr.a
        itr = itr.d

    return res

def prim_mul(args):
    '''
    Variadic multiplication.
    Takes a list of numbers.

    Multiplies all values in the list. (* 1 2 3 4) would
    evaluate to 24.

    If it is a singleton, return itself.
    '''
    if not number_listp(args):
        raise TypeError("Cannot multiply non-numbers.")

    length = args.length()

    if length == 0:
        return Number(1)

    elif length == 1:
        return args.a

    res = args.a
    itr = args.d

    while not nilp(itr):
        res = res * itr.a
        itr = itr.d

    return res


def prim_div(args):
    '''
    Variadic division.
    Takes a list of numbers.

    Multiplies all values in the list. (/ 16 2 2 2) would
    evaluate to 2.

    If it is a singleton, return itself.
    
    If an attempt is made to divide by 0, then Python 
    will complain on our behalf, but an extension 
    might be to check for that.
    '''
    if not number_listp(args):
        raise TypeError("Cannot divide non-numbers.")

    length = args.length()

    if length == 0:
        return Number(1)

    elif length == 1:
        return args.a

    res = args.a
    itr = args.d

    while not nilp(itr):
        res = res / itr.a
        itr = itr.d

    return res

def prim_lt(args):
    '''
    Takes two numbers.

    If the first is less
    than the second, then return t.
    Else, return nil.
    '''
    arity_check(args, 2)

    if not number_listp(args):
        raise TypeError("Cannot compare non-numbers.")

    if args.a < args.d.a:
        return bel_t
    else:
        return bel_nil

def prim_leq(args):
    '''
    Takes two numbers.

    If the first is less
    than or equal to the second, then return t.
    Else, return nil.
    '''
    arity_check(args, 2)

    if not number_listp(args):
        raise TypeError("Cannot compare non-numbers.")

    if args.a <= args.d.a:
        return bel_t
    else:
        return bel_nil


def prim_gt(args):
    '''
    Takes two numbers.

    If the first is greater
    than the second, then return t.
    Else, return nil.
    '''
    arity_check(args, 2)

    if not number_listp(args):
        raise TypeError("Cannot compare non-numbers.")

    if args.a > args.d.a:
        return bel_t
    else:
        return bel_nil

def prim_geq(args):
    '''
    Takes two numbers.

    If the first is greater
    than or equal to the second, then return t.
    Else, return nil.
    '''
    arity_check(args, 2)

    if not number_listp(args):
        raise TypeError("Cannot compare non-numbers.")

    if args.a >= args.d.a:
        return bel_t
    else:
        return bel_nil

def prim_eq(args):
    '''
    Takes two values,
    either they respond true to id,
    then they respond to the same place in memory.
    If they respond to true, the result is true.

    If the first is a pair, and the second is a pair,
    return false.

    If neither is a pair, just return nil.

    If the first and second are both pairs.
    Verify if their cdr is the empty list. (happens in about 30% of the cases)

    If they are not compatible with id,
    they may be pairs.

    Takes two numbers.

    If their values are equal, return t.
    Else, return nil.

    First extension would be to chars and symbols.

    A second extension to consider might be to include pairs (i.e,
    it will crawl the pair / list to ensure every element is equal). 
    This is less strict than identity.
    '''
    arity_check(args, 2)

    # Different types
    if prim_type(args.a).n != prim_type(args.d.a).n:
        print("Different Types")
        return bel_nil

    if prim_type(args.a).n == "number":
        if args.a.v == args.d.a.v:
            return bel_t
        else:
            return bel_nil

    if prim_type(args.a).n == "symbol":
        if args.a.n == args.d.a.n:
            return bel_t
        else:
            return bel_nil

    if prim_type(args.a).n == "char":
        if args.a.n == args.d.a.n:
            return bel_t
        else:
            return bel_nil

    if prim_type(args.a).n == "stream":
        raise ValueError("Streams are unimplemented")

    '''
    A pair is equal to another pair
    if and only if every element in pair A is equal
    pairwise to every element in pair B.

    If it is a pair,
    then compare the cars and cdrs.

    If it is a proper list,
    then traverse the list and compare elements pairwise.

    We have not implemented this yet.
    '''
    if prim_type(args.a) == Symbol("pair"):
        if not nilp(args.a.d) and not nilp(args.d.a.d):
            pass
        raise ValueError("Equality of pairs is unimplemented")

    else:
        raise TypeError("Cannot call equality on non-Bel types")

'''
OTHER PRIMITIVES
'''

def prim_genv(args):
    print(g_env)
    return bel_nil

def prim_define(args):
    '''
    Takes a name (Symbol) and an expression.
    It evaluates the expression and puts it
    on the global environment.

    Unless it exists, at which point
    you override it.
    '''
    arity_check(args, 2)
    if not symbolp(args.a):
        raise TypeError("Cannot call define on non-symbol")
    symbol = args.a
    expr = args.d.a
    symbol_table[symbol.n] = eval(expr, bel_nil)
    print("Symbol defined: %s" % symbol)
    print("Result of eval on expr: %s" % eval(expr, bel_nil))
    return symbol

'''
SPECIAL FORMS
'''

def special_quote(exp, l_env):
    '''
    An expression in the form (quote a)
    should be passed to special quote.

    We can only quote one object, which could be a pair.
    Evaluating an object that has been quoted removes the quote.

    This is key to code as data, et al.
    '''
    n = exp.length()

    if n != 2:
        raise ValueError("We can only quote one object.")

    return exp.d.a

def special_if(exp, l_env):
    body = exp

    if nilp(body.d):
        return eval(body.a, l_env)

    if not pairp(body.d):
        raise SyntaxError("The cdr of this pair has to be a pair.")

    else:
        test = body.a
        conseq = body.d.a

        if not nilp(eval(test, l_env)):
            return eval(conseq, l_env)
        else:
            return special_if(body.d.d, l_env)

def special_dyn(rest, l_env):
    pass

def special_set(clauses, l_env):
    '''
    Set = bounding each vi to the value of ei.
    (set v1 e1 ... vn en)
    '''
    print(clauses)
    syms = bel_nil
    vals = bel_nil

    itr = clauses
    while not nilp(itr):
        sym = itr.a

        if not symbolp(sym) or nilp(sym):
            raise TypeError("You can only bind global bindings with symbols.")

        val = eval(itr.d.a, l_env)

        syms = Pair(sym, syms)
        vals = Pair(val, vals)

        itr = itr.d.d

    while not nilp(syms):
        assign_val(l_env, syms.a, vals.a)
        syms = syms.d
        vals = vals.d

    return bel_nil


def special_define(n, p, e):
    '''
    (define n p e)
    is an abbreviation for
    (set n (lit clo nil p e))

    special_define is never called. The define primitive is handled by calling set with two parameters.
    '''
    return 

def map_quote(args):
    '''
    Takes a Bel list,
    puts quotes on
    all the args within
    the Bel list,
    returns that new
    quoted Bel list.

    (1 2 3) -> ('1 '2 '3)
    '''
    if nilp(args):
        return bel_nil

    else:
        '''
        Has to be a list as what we pass to it is constructed by eval_list.
        '''
        return Pair(Pair(Symbol("quote"), Pair(args.a, bel_nil)), map_quote(args.d))

'''
REPL
Inspired by Peter Norvig
'''

# A prompt-read-eval-print loop.

def repl(prompt='Babybel > '):
    print("Welcome to Babybel. Type quit to exit the REPL.\n")
    g = Reader()
    setup_environment()
    while True:
        inpt = input(prompt)
        try:
            if inpt == "quit": break
            val = eval(g.get_sexpr(inpt), l_env)
            if val is not None: 
                print(val)
        except Exception as e:
                print('%s: %s' % (type(e).__name__, e))

def setup_environment():
    debug = False
    symbol_table = {}
    g_env = symbol_table
    l_env = Env()
    d_env = Env()
    generate_primitives(primitives)

if __name__ == "__main__":
    repl()
else:
    g = Reader()
    setup_environment()

def test(test_string):
    test = g.get_sexpr(test_string)
    print("test: %s" % (test))
    result = eval(test, bel_nil)
    print("result of eval on test: %s \n" % (result))
    return result
    
'''
EOF
'''