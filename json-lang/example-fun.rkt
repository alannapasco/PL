#lang json-lang

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

