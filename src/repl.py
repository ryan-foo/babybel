# REPL module
# Inspired by Peter Norvig

from eval import *
from reader import *

# A prompt-read-eval-print loop.
def repl(prompt='Babybel > '):
    print("Welcome to Babybel. Type quit to exit the REPL.\n")
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
    g = Reader()
    g_env = symbol_table
    l_env = Env()
    d_env = Env()
    generate_primitives(primitives)

repl()
