
## CLASSES and OBJECTS w/o Inheritance

A CJ program consists of a sequence of class definitions plus an
expression (which represents `main`):

- [CDef, ..., CDef, CExp] %% where the `Name`s of classes are pairwise distinct 

where

- CDef is
    ["class", Name, 
     ["field", Type, Name, "=", CExp],
     [Type, Name, [Type, Name], CExp]]
   
  - the initialization expression is in place of a constructor 

- CExp is one of:
   - true
   - false
   - [CExp, "^",  CExp]
   - [CExp, "&&", CExp]
   - [CExp, ">",  CExp]
   - [CExp, "==", CExp]              
   - Integer (small ones)
   - [CExp, "-",  CExp]
   - [CExp, "+",  CExp]
   - Name                                        
   - ["let", "var", Type, Name, "=", CExp, "in", CExp]
   - ["let", "fun", Type, Name, [Type, Name], CExp, "in", CExp]
   - ["call", Name, CExp]	 		
   - ["set", Name, CExp]
   - ["repeat", CExp, "until", CExp] 
   - ["if", CExp, CExp, "else", CExp]   
   - ["new" Name]

- new expression 
   - [CExp, "dot-call", Name, CExp]  %% method call 
   - [CExp, "dot-get", Name]         %% field deref 
   - [CExp, Name, "=", CExp]         %% field set 

- Name is a (simple) String (no JSON tricks).

- Type is one of:
   - "int"
   - "bool"
   - [Type, "->", Type]
   - ["instance", Name]

Grammar constraints that we won't check but that can go wrong if you construct your own examples:

- the names of classes must be pairwise distinct
- the variables mentioned in a class definitions are references to the parameter,
  meaning even inside of classes code must use `["this",field,"=",e]` and `["this", "dot-get", field]`
- the names in a "class" type match those of the class
- the name in an "instance" type matches a class name
- `this` isn't special, so in principle you could assign to it and wipe it out; don't do it 

### Examples

Here is a simple example: 

```
[["class","a%",["field","int","x","=",10],["int","m",["int","y"],["this","x","=","y"]]],
 ["class","b%",["field","bool","x","=",false],["bool","k",["bool","z"],["this","x","=",[["this","dot-get","x"],"||","z"]]]],

 ["let","var",["instance","a%"],"a","=",["new","a%"],"in",
 ["let","var",["instance","b%"],"b","=",["new","b%"],"in",
 ["let","var","bool","_","=",["b","dot-call","k",true],"in",

 [["b","dot-get","x"],
  "^",
  [[["a","dot-call","m",42],"+",["a","dot-get","x"]],
   "<",
   100]]]]]]
```

We should be able to implement a class the encodes a list, but let's skip this for now.

### Stages

1. The grammar consists of mutually recursive productions.
   Your parse will need to consist of mutually recursive functions,
   	one per production. -> Fundamentals I and II

2. This applies to your type checker too.

3. The type checker for programs needs to proceed in two stages:
   - create a "class environment" that maps class names to the declared types of fields and methods
   - then check the "main" expression and the classes in this environment.
   - The point of the class environment is to set up a mutually recursive scope for classes.
   - From it, the TC can retrieve information when it encounters [instance Type].

4. The `value` function needs to turn classes into "vtables" and
   it needs a meaning for objects.
   - a vtable tracks the "constructor" and the code for the methods
   - the meaning of an object consists of the vtable and the field value 


### Question

Human facing: the implementation of the language "knows" one more form
of type, the entries in the class environment. Should this form of
type be exposed to programmers? Will it increase the expressive power
of the programming language?



