package pl;
import com.google.gson.*;
import pl.AST.*;
import pl.Meaning.IMeaning;
import pl.TypePrediction.Type;
import pl.SymbolTable.Accumulator;

import java.util.ArrayList;

public class Main {
    public static void main(String[] args) {

        ArrayList<JsonElement> input = Utils.processJsonExampleSeries("input.json");
        ArrayList<JsonElement> output = Utils.processJsonExampleSeries("output.json");

//        Stream<AST> parsed = input.stream().map(Main::parse);
//        parsed.forEach(System.out::println);

        Utils.testCmp(input, output);
        Main.processWithStaticDistance(input);
    }

    /**
     * Processes (parses -> type checks -> determines the value of) the given input examples
     * by first converting all examples to a staticDistance AST
     */
    public static void processWithStaticDistance(ArrayList<JsonElement> examples) {
        System.out.println("\n---------- SD AST - Parse -> typecheck -> value results: ----------");
        for (JsonElement example: examples) {

	  // MF : in sw dev, I'd turn the body of this loop into a method 

            AST ast = Main.parse(example);

	    // MF : in a "compiler" or "IDE plug-in, you want to run `staticDistance` _after_ type checking 

            AST astWithSD = ast.staticDistance(new Accumulator<>());
            System.out.println("OR AST: " + ast + "\nSD AST: " + astWithSD);

            try {
                ast.typeCheck(new Accumulator<>());
		// MF: swdev: narrow the scope of the try .. catch to just this line 
		//     (because the calls to `value` and `valueSD` do not (yet) raise exns
		//      and when we get there, we want to know that this is where they came from)

		// MF: factored out this next line so I could use it below:
		IMeaning actual = ast.value(new Accumulator<>());
                System.out.println("value:   " + actual);

		/* MF: swdev: 
		   make a checkOK method, like this: 
		   void checkOK(AST ast, IMeaning expected) { 
                     AST astWithSD   = ast.staticDistance(new Accumulator<>());
                     int numLets     = ast.countNumLetsInAST(0);
                     IMeaning[] acc  = new IMeaning[numLets];
		     IMeaning actual = astWithSD.valueSD(acc, numLets-1);
		     if (!actual.equals(expectec)) {
		        throw new Exception("bad SD"); // or better
                     }
                     return ; 
                   }

                   call it with `checkOK(ast, actual)` 
		   when we move on and leave SD behind, comment out this one call 
		 */

                int numLets = ast.countNumLetsInAST(0);
                IMeaning[] acc = new IMeaning[numLets];
                System.out.println("valueSD: " + astWithSD.valueSD(acc, numLets-1));
            } catch (Exception e) {
                System.out.println("Error from Main: " + e);
            }
            System.out.println();
        }
    }


    /**
     * Processes (parses -> type checks -> determines the value of) the given input examples
     */
    public static void process(ArrayList<JsonElement> examples) {
        System.out.println("---------- Parse -> typecheck -> value results: ----------");
        for (JsonElement example: examples) {
            AST ast = Main.parse(example);
            System.out.println("AST:   " + ast);
            try {
                ast.typeCheck(new Accumulator<>());
                System.out.println("value: " + ast.value(new Accumulator<>()));
            } catch (Exception e) {
                //Type Check failed
                System.out.println("Invalid Type Example");
            }
        }
        System.out.println();
    }


    /**
     * Produces an abstract syntax tree which represents a JSONLang program
     */
    public static AST parse(JsonElement element) {
        if (element.isJsonPrimitive()) {
            return Main.parsePrimitive(element.getAsJsonPrimitive());
        } else if (element.isJsonArray()) {
            return Main.parseArray(element.getAsJsonArray());
        }
        return new ASTError("Not Valid AST: " + element + " ");
    }


    /**
     * Parses a single element of JSONLang
     */
    private static AST parsePrimitive(JsonPrimitive element) {
        try {
            int i = element.getAsInt();
            return new ASTInteger(i);
        } catch (NumberFormatException e) {
            //here we could either have a string or boolean
            if (element.getAsBoolean() || element.getAsString().equals("false")) {
                return new ASTBoolean(element.getAsBoolean());
            } else {
                //here we have a String, which at the moment only represents the name of a variable (bc we dont yet have strings in this language)
                return new ASTName(element.getAsString());
            }
        }
        //return new ASTError("Invalid Primitive: " + element + " ");
    }


    /**
     * Parses an array input of JSONLang
     */
    private static AST parseArray(JsonArray expression) {
        if (expression.size()==3) {
            return Main.parseOperation(expression);
        } else if (expression.get(0).getAsString().equals("let")){

            return Main.parseVariableAssignment(expression);
        }
        return new ASTError("Invalid Expression: " + expression + " ");
    }


    /**
     * Parses an operation represented in JSONLang
     */
    private static AST parseOperation(JsonArray operationExpression){
        JsonElement firstVal = operationExpression.get(0);
        String op = operationExpression.get(1).getAsString();
        JsonElement secondVal = operationExpression.get(2);
        switch (op) {
            case "^":
                return new ASTAnd(parse(firstVal), parse(secondVal));
            case "||":
                return new ASTOr(parse(firstVal), parse(secondVal));
            case ">":
                return new ASTGreaterThan(parse(firstVal), parse(secondVal));
            case "-":
                return new ASTSub(parse(firstVal), parse(secondVal));
            case "+":
                return new ASTAdd(parse(firstVal), parse(secondVal));
            default:
                return new ASTError("Invalid Operator in expression: " + operationExpression + " ");
        }
    }


    /**
     * Parses a variable assignment in JSONLang
     */
    private static AST parseVariableAssignment(JsonArray expression){
        String varTypeInput = expression.get(1).getAsString();
        String varName = expression.get(2).getAsString();
        JsonElement varVal = expression.get(4);
        JsonElement varScope = expression.get(6);

        Type varType;
        if (varTypeInput.equals("int")){
            varType = Type.INTEGER;
        } else if (varTypeInput.equals("bool")) {
            varType = Type.BOOLEAN;
        } else {
            return new ASTError("Invalid Variable Assignment Type: " + expression);
        }
        return new ASTLet(varType, varName, Main.parse(varVal), Main.parse(varScope));
    }
}
