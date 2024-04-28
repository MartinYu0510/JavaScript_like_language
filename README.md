# Introduction
This is a language similar to JavaScript implemented by Haskell language with aid of functional programming.

# Implementation 
Apart from using functional programming of haskell to implement this language, I also apply other tools and feature. For example, usage of special data type : Exp(Expression), Lit(Literal), Env(Environment), Clos(Closure)....
Furthermore, lambda expression is used in development of first class function.
Particularly, monad, an interface from haskell, is used to keep the code in a clear and precise structure.

# Feature
1. Arithmetic operation and Logical operation
2. Local and global variable declaration and assignment
3. Top-level function and first class function declaration
4. Support recursive varialbe(function) declaration
5. Utilization of lexical scoping
6. Type checker(support recursion)
7. Evaluation are done witht eh framework of call-by-need framework(or so-called lazy evalution)
