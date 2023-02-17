#lang json-lang


;; a function for setting an int var to a new value 
["let", "var", "int", "x", "=", 1, "in",
      ["let", "fun", "int", "setx", ["int", "nu"], ["x","=","nu"], "in",
            ["let", "var", "int", "_", "=", ["call","setx",2], "in",
                  "x"]]]

;; a functionn for setting a bool var to a new value
["let", "var", "bool", "x", "=", #true, "in",
      ["let", "fun", "bool", "setx", ["bool", "nu"], ["x","=","nu"], "in",
            ["let", "var", "bool", "_", "=", ["call","setx",2], "in",
                  "x"]]]
            
            


;; ---------------------------------------------------------------------------------------------------
;; a simple assignment, returns the original value of 
["let", "var", "int", "x", "=", 1,
      "in",
      ["x","=",2]]

;; a dummy assignment, w/o visible effect 
["let","var","int","x","=",5,
      "in",
      ["let","fun","int","f",["int","y"],["x","+","y"],
            "in",
            ["let","var","int","x","=",42,
                  "in",
                  ["call","f",["let","var","int","_","=",["x","=",["x","+",1]],"in","x"]]]]]

;; the function changes its "closed over" x
["let","var","int","x","=",5,
      "in",
      ["let","fun","int","f",["int","y"],
            ["let","var","int","z","=",["x","=",["x","+",2]],"in",["x","+","y"]],
            "in",
            ["let","var","int","x","=",42,
                  "in",
                  ["call","f",["let","var","int","_","=",["x","=",["x","+",1]],"in","x"]]]]]

;; the function assigns 0 to its parameter, becoming the "identity function" 
["let","var","int","x","=",5,
      "in",
      ["let","fun","int","f",["int","y"],["let","var","int","z","=",["y","=",0],"in",["x","+","y"]],
            "in",
            ["let","var","int","x","=",42,
                  "in",
                  ["call","f","x"]]]]

;; the function assigns to its parameter and the actual argument is a variable
;; call-by-value is NOT call-by-reference 
["let","var","int","x","=",5,"in",
      ["let","fun","int","f",["int","y"],
            ["let","var","int","z","=",["y","=",0],"in",["x","+","y"]],
            "in",
            ["let","var","int","x","=",42,
                  "in",
                  ["let","var","int","z","=",
                        ["call","f","x"],
                        "in",["z","+","x"]]]]]

;; -----------------------------------------------------------------------------

;; a simple example of a function declaration and call
["let", "fun", "int", "g", ["int", "y"], ["y","+", 32],
      "in",
      ["call", "g", 10 ]]

;; a function may refer to a variable declared in its definition context
["let", "var", "int", "x", "=", 5,
      "in",
      ["let", "fun", "int", "f", ["int", "y"], ["x", "+", "y"],
            "in",
            ["call", "f", 10 ]]]

;; a function whose parameter has the same name as a variable declared in
;; its definition context
["let", "var", "int", "x", "=", 5,
      "in",
      ["let", "fun", "int", "f", ["int", "x"], ["x", "+", ["x", "+", 10]],
            "in",
            ["call", "f", "x" ]]]

;; a function that refers to q vqriable (x) declared in its definition context
;; ---- as well as its call context. Which one should it use? 
["let", "var", "int", "x", "=", 5,
      "in",
      ["let", "fun", "int", "f", ["int", "y"], ["x", "+", "y"],
            "in",
            ["let", "var", "int", "x", "=", 42,
                  "in",
                  ["call", "f", "x" ]]]]

;; -----------------------------------------------------------------------------
;; old examples

9999999999999

["let", "int", "x", "=", 42, "in", [[10,"+",11], "<", "x"]] 

["let", "var", "int", "y", "=", 42,
      "in",
      ["let", "var", "int", "x", "=", (10, "+", 11),
            "in",
            "y"]]

["let", "var", "int", "x", "=", 42, "in",
      ["x", "<", 21]]

["let", "var", "int", "x", "=", (10, "+", 5),
      "in",
      "x"]

[#t, "^", #f]

["let", "var", "bool", "x", "=", #true,
      "in",
      [["let", "var", "int", "x", "=", 42,
             "in",
             ["x", "<", 21]],
                            "||",
                            "x"]]

