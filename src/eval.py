import functools
from enum import Enum
import random
import sys
import string
import types
import re
import traceback
import math
import operator as op

'''
BABYBEL
'''

# Beyond my advisor, Professor Olivier Danvy, who has provided me guidance beyond compare, I give my thanks to Lucas Vieira, who implemented Believe (C), and William Annis, who implemented PyLisp, for fortifying my understand of interpreters for Lisp languages.


'''
Useful Constants
'''

debug = True
SYMBOLCANDIDATES = string.digits + string.ascii_letters + '-+*/:&$?='
CHARCANDIDATES = ['\\' + char for char in SYMBOLCANDIDATES]

WS = string.whitespace
SPECIAL = "()`',@"
SYNTAX = WS + SPECIAL

INT = re.compile(r'^[+-]?\d+$')
FLOAT = re.compile(r'^[+-]?(\d+\.\d*$|\d*\.\d+$)')

_nummap = {
    INT: int,
    FLOAT: float
}

'''
Axioms
'''



symbol_table = {}
g_env = symbol_table

'''
A list of all characters. Its elements are of the form (c . b), where
c is a character and b is its binary representation in the form of a 
string of \1 and \0 characters.
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
        if isinstance(self, List):
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
    '''
    There is the fact that it is a symbol,
    then it's the name.

    Symbol, by default you can initialize with nil.
    Alternative: special value, you can look at which
    it's bound to undefined.
    '''
    def __init__(self, name, value = None):
        if [char in SYMBOLCANDIDATES for char in name]:
        # When we instantiate a symbol, every char within that symbol should be a valid symbol.
            self.n = name # variable
            self._val = value # value, ideally we'd initialize to the Bel nil symbol, but we don't have recursive typing in Python.
            # you look up using the python string value of the symbol, not the symbol itself. key difference.
            if name not in symbol_table: #Update symbol table if its not there.
                symbol_table[self.n] = self.v

    @property
    def v(self):
        if self.n == "nil":
            return self
        elif self._val is None:
            return Symbol("nil")
        return self._v

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

    def eq(self, other):
        return self.n == other.n


class Char(Atom):
    '''
    A character is a 8-bit integer. We care about their bit representation.
    They look like this, and we eventually want a table that looks like this.
    The nice thing is we already have them in that representation, up top.
    '''

    def __init__(self, name):
        # if __name__ in CHARCANDIDATES:
            self.n = name

    def __repr__(self):
        return self.n

    def cons(self, item):
        '''
        Stick yourself on the back of an item, and that becomes a pair.
        '''
        return Pair(item, self)
    

bel_nil = Symbol("nil")
bel_t = Symbol("t")
bel_o = Symbol("o")
bel_lit = Symbol("lit")
bel_prim = Symbol("prim")
bel_clo = Symbol("clo")

symbol_table["nil"] = Symbol("nil")
symbol_table["t"] = Symbol("t")
symbol_table["o"] = Symbol("o")
symbol_table["chars"] = chars


# Numbers are not a fundamental Bel type, but implementing them as a Bel type makes our job a lot easier. // Believe

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

# We don't ask too many questions about what you put in a pair, so long as its a Bel Type.

class Pair(BelType):
    def __init__(self, a = bel_nil, d = bel_nil):
        '''
        TODO: We temporarily get rid of the type checking for Bel types because we create environment with Python functions in the variables.
        '''
        # if (a.isbeltype()) and (d.isbeltype()):
        self.a = a
        self.d = d
        # else:
        #     raise TypeError("Type not assigned, please instantiate pair with a BelType")

    def __repr__(self):
        '''
        Print the car of the pair.
        Then look at the cdr of the pair.
            If the cdr of the pair is nil, print ')' and stop.
            If the cdr is an atom, print a dot, then print that atom.
            Otherwise, you write a space and parantheses, print the car.
            And then recursively call repr again.
        '''
        return "(%s)" % print_pair_aux(self.a, self.d)

    def __rawrepr__(self):
        '''
        Dot notation
        '''
        if self.d == bel_nil:
            return "(%s)" % (self.a, self.d)
        else:
            if numberp(self.a) and numberp(self.d):
                return "(%s . %s)" % (self.a.v, self.d.v)
            elif numberp(self.a):
                return "(%s . %s)" % (self.a.v, self.d)
            elif numberp(self.d):
                return "(%s . %s)" % (self.a, self.d)
            else:
                return "(%s . %s)" % (self.a, self.d)

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
    But since we don't have a notion of chars at the moment,
    we will just implement string as its own type.
    '''
    def __init__(self, string):
        self.str = make_string(string)
        
    '''
    Ryan will not fuss with characters that much, Bel String is fine and its a great time saver.

    We can have a print_string to get it in our desired representation to the external user if we want to.
    '''

    '''
    A proper list of characters is called a string, and has a special notation,
    zero or more characters within double quotes.

    "hello world" is a string.
    "hello" is a string.
    "" is a string.
    (\a \b \c) is a string, which can be represented as "abc".

    (Question: And the challenge of \backslash comes in!)
    (Thought (QUESTION): Since \ or backslash is already a reserved keyword in Python, we can do as well to use # instead?)

    Answer: it's up to the language implementer what the # character is.
    
    Strings evaluate to themselves.
    '''

# Streams are non-critical for our exploration purposes.

class Stream(BelType):
    def __init__(self, status):
        status = status
        raise NameError("Unimplemented")

    # Pointer to a raw stream
    # The cache of the stream
    # The amount of the stream that is full.

class StreamStatus(Enum):
    CLOSED = 1
    READ = 2
    WRITE = 3

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

def errorp(x: BelType):
    # Tests if an object is a list in the format (lit err . rest)
    if not pairp(x):
        return 0
    if not idp(x.a, Symbol("lit")):
        return 0
    cdr = x.d
    if not idp(cdr.a, Symbol("err")):
        return 0
    return 1

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

        '''
        Maybe termination error?

        Ages ago, Prof Danvy talked to JMC about the first implementation of Lisp. 0, False, or something else.

        Anything that can be used will be abused.

        You face these kind of choices.
        '''

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
    return (literalp(x) and idp(x.d.a, Symbol("prim")))

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

# Takes multiple args and constructs a Bel List. But these multiple args are NOT a list.
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
        return bel_nil;

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

# TODO: Streams

# Error has no formal specification in Bel, other than "there might be an err function which throws an error in the system".

# TODO: Specify and implement errors.

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

        TODO

        '''
        if self.e == bel_nil:
            '''
            The empty list.
            '''
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

        '''
        Calling nilp on env isn't enough,
        we have to check inside it, and
        ask it to tell us what env.e is.
        '''

    elif atomp(env.e):
        return (bel_nil, False)

    elif not symbolp(symbol):
        raise TypeError("We cannot look up %s, as it is not a symbol" % (symbol))

    else:
        itr = env.e
        while (not nilp(itr)):
            # While we have not hit the end of the env.
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

    - lookup in the lexical scope and dynamic scope, we can use an association list because they tend to be small and you don't have to hold them
        - the assoc list is a time-honored tradition to represent environments (Danvy)
        - not the most efficient space-wise. In practice, variables should be compiled into their lexical offset (or how far they are in the lexical environment.)
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
        # print("%s is in the lexical environment!" % symbol)
        # print("Here is it's value: %s" % value)
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

# LIST OF PRIMITIVES (LOP)

def make_primitive(symbol):
    return make_literal(Pair(bel_prim, Pair(symbol, bel_nil)))

def register_primitive_in_env(x):
    '''
    x is a Python string representation of the primitive.
    This function will take our global env (symbol table) and register the primitives on it.
    '''
    symbol = Symbol(x)
    symbol_table[x] = make_primitive(symbol)

primitives = ["id", "join", "car", "cdr", "type", "xar", "xdr", "sym", "nom", "wrb", "rdb", "ops", "cls", "stat", "coin", "sys", "+", "-", "*", "/", "<", "<=", ">", ">=", "=", "err", "pair?", "cons", "g_env", "apply"]

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

Metacircular evaluator
- we have eval and apply,
and they call themselves mutually,
they will have auxillary functions
and special forms to produce
a working interpreter for a Lisp.
'''

'''
eval, or the evaluation function, takes an expression, identifies what it is, and executes it accordingly.

When a simple application is performed, we take a list and consider the first element the symbol that the function is bound to.

We evaluate every element of the list, including the function, before applying the closure to the rest of the evaluated elements, which then will be passed as arguments to the function.

The closure captures the lexical env of when it is evaluated.
'''

def eval(exp, l_env):
    # if not (isinstance(exp, BelType)):
        # raise TypeError("%s is Not a Bel Type" % exp)
    # print("Expression being evaluated: %s" % exp)
    # print("Lexical environment: %s" % l_env)

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
            # We will call special_if on the cdr of the expr, since we already know that it's a pair, and that the first half of the pair is if..
            # We will eval_tp on the same expression,
            # trusting eval_tp to handle it with an iterative
            # approach.
            return eval_tp(exp, l_env)

        elif idp(exp.a, Symbol("set")):
            return special_set(exp.d, l_env)

        elif idp(exp.a, Symbol("define")):
            return special_set(exp.d, l_env)

        elif idp(exp.a, Symbol("def")):
            print("yes, special def")
            return special_def(exp.d, l_env)

        # Otherwise it is an application of a function.
        else:
            '''
            Anything that calls eval recursively
            will not be done in a straight recursive 
            manner.
            Instead, we call eval_tp, which evaluates
            the body of a closure. This instantiates
            a while loop that...
            '''
            return function_apply(eval(exp.a, l_env),
                     eval_list(exp.d, l_env))

    raise Error("%d is not a proper list, you cannot apply a function." % exp)


'''
eval_tp is a new function,
that is called to evaluate the body of a closure.

A Bel expression is ordinarily evaluated through recursive calls to eval,
while tail-recursive sub-expressions are evaluated through the iteration
of the while loop in eval_tp.
'''

def eval_tp(exp, l_env):
    while(True):
        # print("Eval TP")
        # print("Expression being evaluated: %s" % exp)
        # print("Lexical environment: %s" % l_env)
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
                '''
                Test
                '''
                body = exp.d

                if nilp(body.d):
                    # Then it's car is the final else branch of the if expression.
                    exp = body.a
                    continue

                if not pairp(body.d):
                    raise SyntaxError("The cdr of this pair has to be a pair.")

                else:
                    # If the cdr of the body is a pair, then
                    # The car of that pair is the test, and the cadr is the consequent branch.
                    test = body.a
                    conseq = body.d.a
                    # print("IF: test: %s" % test)
                    # print("IF: conseq: %s" % conseq)

                    # We evaluate the test, with a non-tail call to eval.
                    if not nilp(eval(test, l_env)):
                        # print("Eval the consequent. We have passed the test.")
                        # If the test evaluates to a truthy value (not nil), then return evaluation of the consequent.
                        exp = conseq
                        continue
                    # Otherwise, we tail call eval on the cadr on the pair, or the then branch.
                    else:
                        # print("Tail call on the else* branch.")
                        # Else* branch.
                        # print("Else branch: %s" % body.d.d)
                        if nilp(body.d.d.d):
                            exp = body.d.d.a # Else branch: exp
                            continue
                        else:
                            # The if-then-else is not finished, so construct an if expression.
                            exp = Pair(Symbol("if"), body.d.d)
                            continue
                        '''
                        It iteratively treats the if.
                        '''

            elif idp(exp.a, Symbol("set")):
                return special_set(exp.d, l_env)

            elif idp(exp.a, Symbol("define")):
                return special_set(exp.d, l_env)

            elif idp(exp.a, Symbol("def")):
                return special_def(exp.d, l_env)
                '''
                def: function definition
                (def n p e)
                is an abbreviation for
                (set n (lit clo nil p e))
                '''

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
                fun = eval(exp.a, l_env)
                args = eval_list(exp.d, l_env)

                a_fun = fun
                a_args = args

                while (primitivep(a_fun) and (idp(a_fun.d.d.a, Symbol("apply")))):
                    arity_check(a_args, 2)
                    a_fun = a_args.a
                    a_args = a_args.d.a

                if closurep(a_fun):
                    l_env = a_fun.d.d.a # current lexical environment
                    params = a_fun.d.d.d.a # actual parameters
                    body = a_fun.d.d.d.d.a # function body (value that the fn returns)
                    new_env = Env(bind(params, a_args, l_env))
                    exp = body
                    l_env = new_env

                elif primitivep(a_fun):  # guaranteed not to be apply
                    return apply_primitive(a_fun.d.d.a, a_args)

                else:
                   raise TypeError("Not a function")

        else: 
            raise TypeError("%d is an unidentified function object (UFO)." % exp)

'''
APPLY

"is the application function. It takes a function and applies that to the list of evaluated arguments.

A function can be a primitive but also a literal closure.

We bind arguments to the formal parameters, create an extended environment, and evaluate under the new environment."
'''

def bind(params, args, l_env):
    # print("Lexical Env: %s" % l_env)
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

            '''
            DECISION POINT: (potential point for heated discussion)
                Arity mismatch: not enough args.
                Or we could pad them / bind them with nil.
                Or if there are too many args, we can ignore them.
            "Worse is Better vs The Right Thing"
            '''

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

        We create a new function class that performs
        the arity check.
        '''
        return apply_primitive(
            fun.d.d.a, args)

    elif closurep(fun):
        l_env = fun.d.d.a # current lexical environment
        params = fun.d.d.d.a # actual parameters
        body = fun.d.d.d.d.a # function body (value that the fn returns)

        new_env = Env(bind(params, args, l_env))

        if errorp(new_env):
            return new_env

        return eval_tp(body, new_env) # evaluate the body given the new extended environment

    else:
        raise TypeError("Not a function")

@functools.lru_cache(maxsize=None)
def eval_list(bel_list, l_env):
    if nilp(bel_list):
        return bel_nil

    eval_head = eval(bel_list.a, l_env)

    eval_rest = eval_list(bel_list.d, l_env)

    return Pair(eval_head, eval_rest)


'''
when do we stop evaluating? - we stop when we hit nil.
if we hit nil, we add nil to our list, break.
otherwise, we continue evaluating the head of to_eval,
and adding that to our existing construction (eval_result). 
(we construct the evaluated list on the fly).
'''

@functools.lru_cache(maxsize=None)
def eval_list_tp(bel_list, l_env):
    eval_result = eval(bel_list.a, l_env)
    to_eval = bel_list.d
    while(True):
        if nilp(to_eval):
            eval_result = Pair(eval_result, bel_nil)
            break
        else:
            print("to eval: %s" % to_eval)
            eval_result = Pair(eval_result, eval(to_eval.a, l_env))
            to_eval = to_eval.d # proceed down the list
            print("to eval after .d: %s" % to_eval)
            continue
    return eval_result


'''
PRIMITIVE FUNCTIONS

When you look up these symbols (id, join, '+' -- you are returned with "lit prim +" in the symbol table). The environment has been updated by generate_primitives, which registers these primitives. This leads us to the primitive branch in eval, which then tells us to call these primitives accordingly.
'''

def lookup_prim(sym, lit):
    '''
    Takes the symbol,
    and helps point us
    to the right function
    to return.
    '''
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

    Possible extension: error-handling could stop at the REPL level,
    not at the meta-level. Right now we lean on Python's error handling,
    but in an ideal world we would have implemented native error handling
    for Bel.

    Hence, we declare the Bel interpreter as not to be used in anger, or a prototype --  (an  expression that tells us that it is not to be used for production level code).
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
    # elif lookup_prim(sym, "list"):
    #     pass
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
    # print("Checking type of: %s" % args)
    arity_check(args, 1)
    if isinstance(args, Pair):
        args = args.a

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
    val = args.d.a # the head, of the rest...
    if not pairp(pair):
        raise TypeError("%s is not a pair" % pair)
    else:
        pair.a = val
    return val

def prim_xdr(args):
    arity_check(args, 2)
    pair = args.a
    val = args.d.a # the head, of the rest...
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
    '''
    Unimplemented: Bel to Python String.
    '''

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

    The second argument is a Bel list. It may not be proper, it doesn't matter.

    The result of eval_list is a Bel list.

    Apply the first argument to the second.

    ev_list

    Single fixpoint operator you can apply
    to any function of any arity...
    
    We implement a bounded form of apply
    that takes two arguments.

    First, we check it takes two args.

    We trust that when we apply the function,
    whatever we are applying will check its own arguments.

    '''
    arity_check(args, 2)
    a_fun = args.a
    a_args = args.d.a

    if primitivep(a_fun):    
        return apply_primitive(
            a_fun.d.d.a, a_args)

    elif closurep(a_fun):
        # print("We are in a closure.")
        l_env = a_fun.d.d.a # current lexical environment
        params = a_fun.d.d.d.a # actual parameters
        body = a_fun.d.d.d.d.a # function body (value that the fn returns)
        # print("Current lexical environment: %s" % l_env)
        new_env = Env(bind(params, a_args, l_env))

        # print("We are evaluating a closure with the body: %s" % body)
        # print("We are evaluating a closure, with the lexical environment of %s" % new_env)

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

    # Broken, fix on variadic numbers. 

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
    '''
    if prim_type(args.a) == Symbol("pair"):
        print("We are comparing a pair")
        if not nilp(args.a.d) and not nilp(args.d.a.d):
            pass
        raise ValueError("Equality of pairs is unimplemented")

    else:
        raise TypeError("Cannot call equality on non-Bel types")

'''
OTHER PRIMITIVES
'''

# With thanks to Luke Vieira for the error format.

def prim_err(args):
    string = args.a
    if not stringp(string):
        return make_error(String("First argument of error must be a string"), bel_nil)
    return make_error(string, args.d)

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
        # Then it's car is the final else branch of the if expression.
        return eval(body.a, l_env)

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
            return eval(conseq, l_env)
        # Otherwise, we tail call eval on the cadr on the pair, or the then branch.
        else:
            # print("Tail call on the then branch.")
            # Then branch.
            # print("Then branch: %s" % body.d.d)
            return special_if(body.d.d, l_env)

def special_if_itr(exp, l_env):
    '''
    The while loop evaluates the expression denoted by tp.

    At the same level of the control stack,
    we are moving through the expression.

    The simplest way to make a tail recursive function
    is to write it iteratively.

    You are only extending the lexical environment,
    which is temporary until the next call.

    Afterwards, you extend the original lexical environment...
    '''
    pass


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


def special_def(args, l_env):
    '''
    (def n p e)
    is an abbreviation for
    (set n (lit clo nil p e))
    '''
    sym = args.a
    p = args.d.a
    e = args.d.d.a
    rest = make_list(p, e)
    lit_clo = Pair(sym, Pair(make_closure(bel_nil, rest)))
    return special_set(lit_clo, l_env)


def let(bindings, body):
    '''
    bindings = ((name . expr) (name . expr) (name . expr))
    body = 
    (let ((x 1)) (f x)) = ((fn (x) (f x)) 1)
    (let ((x 1) (y 2)) (+ x y)) = ((fn (x y) (+ x y)) 1 2)

    The formal (x y) becomes the formals, (x y)
    the definiens (1 and 2) become the actuals (1 2), or the args
    The body becomes this body.. etc

    Landin's Correspondence
    A block structure and function applications mean the same.
    '''
    pass

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

# EOF