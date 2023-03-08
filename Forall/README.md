
## Parametric Types (aka Generics) 

- FExp is one of:
   - true
   - false
   - [FExp, "^",  FExp]
   - [FExp, "&&", FExp]
   - [FExp, ">",  FExp]
   - [FExp, "==", FExp]              
   - Integer (small ones)
   - [FExp, "-",  FExp]
   - [FExp, "+",  FExp]
   - Name                                        
   - ["let", "var", Type, Name, "=", FExp, "in", FExp]
   - ["let", "fun", Type, Name, [Type, Name], FExp, "in", FExp]
   - ["set", Name, FExp]
   - ["repeat", FExp, "until", FExp] 
   - ["if", FExp, FExp, "else", FExp]   

- revised syntax
   - ["call", FExp, FExp] 

- new expressions:
   - ["let", "tfun", [TypeName], Type, Name, [Type, Name], FExp, "in", FExp]
   - ["tcall", FExp, Type]

- Type is one of:
   - "int"
   - "bool"
   - [Type, "->", Type]

- new types:
   - TypeName 
   - ["forall", TypeName, Type]
   
- Name is a (simple) String (no JSON tricks).

- TypeName is just a name. (If you're comfortable with Greek letters, use those for readability.)

### Example

_Problem_ 

Abstract the two `expt-` functions in the following 'program' into a
single function and use it instead:  

```
["let","var","int","start","=",42424242424242424, "in",
["let","fun","bool","true?",["bool","x"],"x","in",
["let","fun","bool","not",["bool","x"],["if","x",false,"else",true],"in",
["let","fun","bool","zero?",["int","x"],["x","==",0],"in",
["let","fun","int","sub1",["int","x"],["x","-",1],"in",
["let","fun","int", "expt-i",["int", "x"],["repeat",["x","=",["call","sub1","x"]],"until",["call","zero?","x"]],"in",
["let","fun","bool","expt-b",["bool","x"],["repeat",["x","=",["call","not", "x"]],"until",["call","true?","x"]],
"in",
 [["call","expt-b",false],
  "||",
  [["call","expt-i",10],"<",22]]]]]]]]]
```

_Solution_ 

```
["let","var","int","start","=",42424242424242424,"in",
["let","fun","bool","true?",["bool","x"],"x","in",
["let","fun","bool","not",["bool","x"],["if","x",false,"else",true],"in",
["let","fun","bool","zero?",["int","x"],["x","==",0],"in",
["let","fun","int","sub1",["int","x"],["x","-",1],"in",
["let","tfun",["α"],[["α","->","bool"],"->",["α","->","α"]],"expt",[["α","->","α"],"exec"],
                    ["let","fun",["α","->","α"],"expt-2",[["α","->","bool"],"stop?"],
			         ["let","fun","α","expt-3",["α","x"],
				               ["repeat",["x","=",["call","exec","x"]],"until",["call","stop?","x"]],
		                  "in",
				  "expt-3"],
      		     "in",
      		     "expt-2"],
"in",
 [["call",["call",["call",["tcall","expt","bool"],"not"],"true?"],false],
  "||",
  [["call",["call",["call",["tcall","expt","int"],"sub1"],"zero?"],10],"<",11]]]]]]]]
```

### Stages

1. Parsing 1: Let's make function calls more flexible first: 

   - Modify the parser to accommodate the new `call` syntax first.
   - This will require a small change to the type checker and the `value` method. 

2. Parsing 2: Then add the new expression forms and types to the parser. You will
   also need new AST variants and Type variants. 
   
3. Type Checking needs new rules for the new expressions. 
   - The generalized function call syntax should already work as is. 

4. Modify `value`. Turns out, this is a noop. 

Details follow. 

### Scope 

- The scoping rules for the expression language don't change. 
- The scoping rules for the type language are as expected. 

In 
```
["let","tfun",["α"], rt, FName, [at, AName], rhs, "in", scope]
```
types may use "α" in `rt`, `at`, `rhs`, and `scope`. 



### Type Checking

1. constraint that we won't check but that can go wrong if you construct your own examples: 

   - a type that contains a name that is not _declared_ with a ∀, example: (∀ α β)

2. the basic two rules: 

   - ["tfun", [α], rt, f, [at, x], rhs, "in", body]
   - `f` is given the type `(∀ α (at -> rt))` while type checking rhs and body (recursion!) 
   - then the type of the entire expression is the type of body 

   - ["tcall", f, t]
   - `f` must be of type (∀ α t1) (and α is not contained in t)
   - then the type of the entire expression is t1 with all α replaced by t
   - (_Because of the constraint, this is non-trivial. Assume for now it's always true._)

### Evaluation

Types don't exist at run time: 

- `["tfun", [α], rt, f, [at, x], rhs, "in", body]`
- ignores the types;
- creates a recursive closure for `f(x) = rhs`
- run body with `f` bound to that closure 

- `["tcall", f, a]`
- evaluates `f` and ignores `a` 

