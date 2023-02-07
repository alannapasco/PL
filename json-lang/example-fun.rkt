#lang json-lang

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

["let", "fun", "int", "g", ["int", "y"], "y",
      "in",
      ["call", "g", 10 ]]

["let", "var", "int", "x", "=", 5,
      "in",
      ["let", "fun", "int", "f", ["int", "y"], ["x", "+", "y"],
            "in",
            ["let", "var", "int", "x", "=", 42,
                  "in",
                  ["call", "f", 10 ]]]]