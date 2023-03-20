
## CLASSES and OBJECTS w/o Inheritance

### Grammar 

A CJ program consists of a sequence of class definitions followed by one expression:

- [CDef, ..., CDef, CExp] %% where the `Name`s of classes are pairwise distinct 

where

- CDef is
    ["class", Name, 
     ["field", Type, Name, "=", CExp],
     [Type, Name, [Type, Name], CExp]]
   
  - the initialization expression is in place of a constructor 

- CExp is one of:
   - ... the old ones ... 

- new expression 
   - `["new", Name]`
   - `[CExp, "dot-call", Name, CExp]`  %% method call 
   - `[CExp, "dot-get", Name]`         %% field deref 
   - `[CExp, Name, "=", CExp]`         %% field set 

- Type is one of:
   - ... the old ones ...

- new types    
   - ["instance", Name]

Grammar constraints that we won't check but that can go wrong if you construct your own examples:

- the names of classes must be pairwise distinct
- the name in an ["instance", Name] type matches a class name

Also:

- `this` isn't special, so in principle you could assign to it and wipe it out; don't do it 
- the variables mentioned in a class definitions are references to parameter only,
  meaning even inside of classes code must use `["this",field,"=",e]` and `["this", "dot-get", field]`

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

### Informal Meaning

A CJ program is pretty much like a Java program

- with `main` represented by the expression
- without a constructor because fields must be initialized
- plus real (first-class) functions rather than static methods

### Types

- A `["new", Name]` expression is of type `["instance", Name]`.
- Method calls, field dereferences, and field sets must have an expression of type `["instance", Name] in the first position.
- Type checking benefits form adding an _internal_ type of the shape:

```
 ["class",
   Type, Name,
   Type, Name, [Type]]
```

Then the type checker keeps a (hash)map from `className` to one of these types. 

### Meaning

- The `value` function needs an "meaning" for every class. It's like the "closure_aka_delayed..." IMeaning,
  keep track of the method def. and the field initialization expression. 
- The sequence of classes is turned into a (hash)map from the class names to these "meanings". 
- `value` needs an IMeaning for every object, the result of a `new` expression,
  which is the field combined with a (pointer to the) class
  "meaning". This second part is called a _vtable_; having it around
  means constant-time calls to methods. In this context,
  - how does `new`  initialize the field? 
  - what does a field deref expression retrieve?
  - what does a field mutation expression compute?
  - how does a method call work?
  
### Stages

1. The grammar consists of mutually recursive productions.
   Your parse will need to consist of mutually recursive functions,
   	one per production. -> Fundamentals I and II

2. This applies to your type checker too.

3. The type checker for programs needs to proceed in two stages:
   - create a "class environment" that (hash)maps class names to the declared types of fields and methods.
   - then check the "main" expression and the classes in this environment.
   - The point of the class environment is to set up a mutually recursive scope for classes.
   - From it, the type-checker can retrieve relevant finformation when it must check ["instance", Type].

4. The `value` function needs to turn classes into "vtables" and
   it needs a meaning for objects.
   - create a "class environment" that (hash)maps class names to their "meaning" 
   - then evaluates the "main" expression 
   - The point of the class environment is to set up a mutually recursive scope for (the meaning of) classes.
   - But this table is consulted only once, when an object is created an a pointer to a meaning is needed. 

### Question

Human facing: the implementation of the language "knows" one more form
of type, the entries in the class (type and meaning) environments. 

- Should this form of type/meaning be exposed to programmers?
- Will it increase the expressive power of the programming language?




