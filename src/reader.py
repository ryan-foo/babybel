# Parsing Module

from __future__ import division
import re, sys, io
from eval import *

'''
The job of the parser is to give us a set of tokens,
which we can then pass to eval.

Now mostly functioning. Adapted from William Annis's PyLisp reader, on his website (biostat.wisc.edu).
'''

class Reader:
    def __init__(self, str=None):
        self.str = str
        self.i = 0
        self.len = 0
        self.sexpr = []
        self.pounds = {}   # '#?' reader helper functions

        if str:
            self.sexpr = self.get_sexpr()

    def add_pound_helper(self, char, helper):
        self.pounds[char] = helper

    def get_token(self):
        if self.i >= self.len:
            return None

        # Munch leading whitespace.
        while self.i < self.len and self.str[self.i] in WS:
            self.i = self.i + 1

        if self.i == self.len:
            return None

        # Now, tokenize.
        if self.str[self.i] == '#':
            # Look ahead to get the second character of the pound escape
            # then sling on the next token for special treatment.
            self.i = self.i + 2
            return self.pounds[self.str[self.i - 1]](self.get_token())
        if self.str[self.i] in SPECIAL:
            self.i = self.i + 1
            #print "\tSPECIAL:", self.str[self.i - 1], "\t", self.i, self.len
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
            self.i = self.i + 1               # Remove trailing quote
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

            #print "\tTOK:", tok, "\t", self.i, self.len
            # If the thing is a number, convert it.
            '''
            TODO: Consider removing, since we are trying
            to coerce tokens to integers using Python 
            and comparing them. This would possibly
            lead to bugs when handling floats, but
            this stops numbers in Python strings from
            being considered symbols.
            '''
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
                    # Start parsing again.
                    self.i = self.i - 1
                    expr.append(self.get_sexpr())
                elif tok == "'":
                    expr.append(make_list(Symbol("quote"), self.get_sexpr()))
                elif tok == "`":
                    expr.append(make_list(Symbol("iquote"), self.get_sexpr()))
                # if token is '\', then the next character will be a character
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

class UnboundSymbolError(BaseException):
    pass

if __name__ == '__main__':
    tests = ['(a b c)',
             '(a (b c) d e)',
             '(a "b whaoo" c)',
             'freddie',
             "'frood",
             '(a (b (c (d (e (f (g)))))))',
             "(a 'froo b)",
             "(rule sec-1 (if (> load 1.5) (== host 'wazor) then (boo)))"
             ]
    g = Reader()
    # for test in tests:
    #     print(test, ":", g.get_sexpr(test))


'''
Parser Tests
'''

g = Reader()
test_parser = False

if test_parser:
    print("Get s_expr from '(+ 5 7)'")
    print(g.get_sexpr('(+ 5 7)')) # > ['+', '5', '7'] :expected: 12
    print(g.get_sexpr('(- (+ 10 10) 5)')) # > ['-', ['+', '10', '10'], '5'] - expected: 15
    print(g.get_sexpr('(a); hello world')) # > [(a)] ; comments work!
    print(g.get_sexpr('a')) # > a, Symbol
    print(g.get_sexpr('greetings'))
    print(g.get_sexpr('`a')) # > (iquote a)
    print(g.get_sexpr("'(a b c)")) # > (quote . ((a . (b . (c . nil))) . nil)) or '(A B C)
    print(g.get_sexpr("'(a)")) # > a
    print(g.get_sexpr('"hello"')) # > 'hello' as a Bel string
    print(g.get_sexpr('\\s'))
    # print(g.get_sexpr('#hello')) # failed test, we should print a backquote?
    print(g.get_sexpr("''x"))
    # Should print: (q . (( q . (x . nil)) . nil))
    print(g.get_sexpr("'quote"))
    [print(g.get_sexpr(test)) for test in if_tests]




'''
Representing the character backslash:
Double backslash.

It will not be a separator.

for example with backslash(bel), you read the next tokens until the next separator, (space) or ).
'''
# print(type(g.get_sexpr("\\")))
# print("\h\e\l\l\o")
# print(g.get_sexpr("\h\e\l\l\o"))



'''
I will assume for now that this (borrowed) parser gives us well formed token test cases for eval.

TODO: Characters, and see if we need to do a workaround for characters. I.E: Perhaps not have them at all?
'''
