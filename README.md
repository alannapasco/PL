# PL -- Directed Study

Language Concepts, Language Features
------------------------------------

- basic arithmetic
- types and type checking 
- variable declarations and scope
- function declaration, more scope, more types
  - variable declaration from "immediate function application"
  - developer-facing: why is "static scope" better than "dynamic scope"
- assignment statements, call-by-value 
  - simply typed variables only 
- the programs still terminate 

- higher-order and recursive functions, more types 
  - recursion from assignment *
  - programs may diverge, but the type checker will terminate

- conditionals and loops,
  recursive functions vs loops (measured)

- parametric polymorphism: forall types 

- classes -- object as instance (Java) 

- specification vs implementation; "meta" properties 
  - duality of language features: introduction, elimination
    - arithmetic seems to stand out 
    - "set" stands out 
  - typing rules vs logic vs type checker 
  - theorems (meta theorems)
    - type checking predicts perfectly (exn, termination)
  - "compile time" (types, static distance, removal of casts)
    vs
    "run time" (value)

More Language Features (?)
----------------------

  - call-by-reference, different from call-by-name; aliasing

  - statements -- add Void to Type 

  - exceptions -- add / or arrays, buffer overflow 
  - exception handlers -- meta handlers (??)

  - gradual typing
    - a la TypeSscript 
    - a la TypedRacket 

  - objects -- what's a higher-order function? (JavaScript, pure)

  - syntactic abbreviations?

Meta Interpretation Obscures True Meaning
-----------------------------------------

- abstract machines: everything becomes explicit 
  - not quite virtual machines 
- CEK machine for 'pure' json-lang
- CESK machine for json-lang with assignment statements


