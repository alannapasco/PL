
Conditionals, Loops, Loops vs [Tail-]Calls 

Add the following new expressions: 

- [J, "==", J]              -- equality of ints 
- ["repeat", J, "until", J] -- returns last result of j-1 before j-2 evaluates to true 
- ["if", J, J, "else", J]   -- an if _expression_; same type for both branches; result of branch

Run these two programs, both return 1:

1. an imperative repeate program

```
["let","var","int","x","=",100,
  "in",
  ["repeat",
    ["x","=",["x","-",1]],
   "until",
    ["x","==",0]]]
```    

2. a functional program that expresses the same `repeat` computation 

```
["let","var","int","x","=",100,
  "in",
  ["let","fun","int","body",["bool","_"],["x","=",["x","-",1]],
   "in",
   ["let","fun","bool","condition",["bool","_"],["x","==",0],
    "in",
    ["let","fun","int","repeat",["bool","_"],
            ["let","var","int","x","=",["call","body",true],
	      "in",
	      ["if",["call","condition",true],
	           "x",
		"else",
		["call","repeat",true]]],
      "in",
      ["call","repeat",true]]]]]
```

-- Measure the maximum size of the "environment stack".

