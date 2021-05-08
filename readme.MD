# Babybel

## Overview

Babybel is a Bel interpreter that implements a subset of Paul Graham's specifications, or a Bel without whistles.

It consists of a lexer and tokenizer -- which parses a string (a Bel program) and returns the output of the Bel program. We aim to bootstrap a minimal Bel interpreter.

We detail this process in the included report: `LispInThe21stCentury.pdf`, as well as a poster, and a suite of tests.

## Goals

Have a Bel REPL, with no macros and streams.

In Babybel, we have implemented a properly tail recursive interpreter for a subset of Bel.
We also demonstrate Church encoding, or Peano numbers as one of the test programs, as well as variadic functions.

## How to use

Run `python3 src/repl.py`, which starts a read-eval-print-loop with the below mentioned programs.

#### Project Structure

`parse.py` contains the Reader class, which is responsible for tokenizing a Python string representation of a Bel program, and returning the S-expression for eval to consume.

`eval.py` contains the bulk of Babybel, implementing the Bel types and predicates, and the famed `eval`. Eval takes S-expressions, and evaluates them, in order to return us the program output.

`env.py` contains the Environment, which `eval` will use to look up symbols etc.

## What this is not

A Bel interpreter ready to be used for production, or "in anger".

- We make no guarantees about speed. Please do not use this as anything beyond a toy, at the moment. However, I hope you find this project an interesting didactic tool.

- Complete.
    - We miss certain axioms.
    - Data Types are not complete: streams, macros.
    - Error recovery.

However, it marks a reasonable attempt to understand and parse what is helpful, not just about Lisp languages, but Bel, and to have a better understanding of what goes on under the hood when we program.

"There is little compile time checking, there are few module systems, there is little safety or discipline built into the language. Babybel is an "anarchic" language, while most other languages are "fascist" (as hackers would have it [Raymond, 1991])." -- Steele and Gabriel, Evolution of Lisp.

## MIT License

This software’s code is distributed under the MIT License, Copyright (c) 2020-2021 Ryan Foo.

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the “Software”), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.