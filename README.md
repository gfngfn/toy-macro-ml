# An implementation of MacroML

This is an implementation of *MacroML* \[1\]. It consists of

* a type checker for MacroML (which also performs the translation of MacroML programs to *MetaML* \[2\]), and
* a naïve back-end interpreter of MetaML.

The syntax and the type system are naturally extended from the original version as to macro parameters. See the paper below \[1\] for detail:

> 1. Steve Ganz, Amr Sabry, and Walid Taha. [Macros as multi-stage computations: Type-safe, generative, binding macros in MacroML](https://dl.acm.org/citation.cfm?id=507646). In _Proceedings of the International Conference on Functional Programming (ICFP’01)_, pages 74–85, 2001.
> 2. Walid Taha and Tim Sheard. [MetaML: Multi-stage programming with explicit annotations](https://dl.acm.org/citation.cfm?id=259019). In _Proceedings of the Symposium on Partial Evaluation and Semantic-Based Program Manipulation (PEPM’97)_, pages 203–217, 1997.


## How to Compile

Under the condition that `make` and `dune` are installed, invoke:

~~~sh
$ make
~~~

and then the executable file `./main` will be created.


## Usage

Just invoke:

~~~sh
$ ./main <source-file>
~~~

and then you can see on stdout:

~~~sh
Type: <type>
Result1: <code-generated-by-macro-expansion>
Result0: <result-of-running-the-code>
~~~


## Syntax

~~~
an expression:
  e ::=
    | '(' e ')'
    | b                              (a Boolean value)
    | n                              (an integer)
    | x
    | 'fun' '(' x ':' ty ')' '->' e
    | e e
    | 'let' x '=' e 'in' e
    | 'letrec' x '=' e 'in' e
    | 'if' e 'then' e 'else e
    | 'letmac' x ps '=' e 'in' e     (a macro definition)
    | x '!' '(' as ')'               (a macro application)
    | '~' e                          (so-called an escape)
    | '@' e                          (so-called a bracket)

a non-empty sequence of macro parameter(s):
  ps ::= p | p ',' ps

a macro parameter:
  p ::=
    | '~' '(' x ':' ty ')'    (an early parameter)
    | x ':' ty                (a late parameter)
    | x ':' ty '->' x ':' ty  (a binder/bindee parameter)

a non-empty sequence of macro argument(s):
  as ::= a | a ',' as

a macro argument:
  a ::=
    | '~' e     (an early argument)
    | e         (a late argument)
    | x '->' e  (a binder/bindee argument)

a monomorphic type:
  ty ::=
    | '(' ty ')'
    | 'bool'
    | 'int'
    | ty '->' ty
    | '@' ty      (a code type)
~~~
