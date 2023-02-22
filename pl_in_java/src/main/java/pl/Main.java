package pl;
import com.google.gson.*;
import pl.AST.*;
import pl.Meaning.IMeaning;
import pl.TypePrediction.VarType;
import pl.SymbolTable.Accumulator;

import java.util.ArrayList;

public class Main {
    public static void main(String[] args) {

        ArrayList<JsonElement> input = Utils.processJsonExampleSeries("input.json");
        ArrayList<JsonElement> output = Utils.processJsonExampleSeries("output.json");

        Utils.testCmp(input, output);
        Main.process(input);
        //Main.processWithStaticDistance(input);

    }

    /**
     * Processes (parses -> type checks -> determines the value of) the given input examples
     * by first converting all examples to a staticDistance AST
     */
    public static void processWithStaticDistance(ArrayList<JsonElement> examples) {
        System.out.println("\n---------- SD AST - Parse -> typecheck -> value results: ----------");
        for (JsonElement example: examples) {
            AST ast = Main.parse(example);
            AST astWithSD = ast.staticDistance(new Accumulator<>());
            System.out.println("OR AST: " + ast + "\nSD AST: " + astWithSD);

            try {
                ast.typeCheck(new Accumulator<>());
                System.out.println("value:   " + ast.value(new Accumulator<>()));

                int numLets = ast.countNumLetsInAST(0);
                IMeaning[] acc = new IMeaning[numLets];
                System.out.println("valueSD: " + astWithSD.valueSD(acc, numLets-1));
            } catch (Exception e) {
                System.out.println("Error: " + e);
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
                System.out.println("Value: " + ast.value(new Accumulator<>()));
            } catch (Exception e) {
                System.out.println("Error: " + e);
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
                return new ASTName(element.getAsString());
            }
        }
    }


    /**
     * Parses an array input of JSONLang
     */
    private static AST parseArray(JsonArray expression) {
        String action = "";
        try {
            action = expression.get(0).getAsString();
        } catch (IllegalStateException e) {
            //
        }

        if (action.equals("let")) {
            if (expression.get(1).getAsString().equals("var")) {
                return Main.parseVariableAssignment(expression);
            } else if (expression.get(1).getAsString().equals("fun")) {
                return Main.parseFunction(expression);
            }
        } else if (action.equals("call")) {
            return Main.parseFunctionCall(expression);
        } else if (expression.size()==3) {
            return Main.parseOperation(expression);
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
            case "=":
                return new ASTSet(firstVal.getAsString(), parse(secondVal));
            default:
                return new ASTError("Invalid Operator in expression: " + operationExpression + " ");
        }
    }


    /**
     * Parses a variable assignment in JSONLang
     */
    private static AST parseVariableAssignment(JsonArray expression){
        String varTypeInput = expression.get(2).getAsString();
        String varName = expression.get(3).getAsString();
        JsonElement varVal = expression.get(5);
        JsonElement varScope = expression.get(7);

        VarType varType;
        try {
            varType = Main.determineType(varTypeInput);
        } catch (Exception e) {
            return new ASTError(e.toString() + expression);
        }

        return new ASTLet(varType, varName, Main.parse(varVal), Main.parse(varScope));
    }


    /**
     * Parses a function in JSONlang
     */
    private static AST parseFunction(JsonArray expression){
        String returnTypeStr = expression.get(2).getAsString();
        String funName = expression.get(3).getAsString();
        String argTypeStr = expression.get(4).getAsJsonArray().get(0).getAsString();
        String argName = expression.get(4).getAsJsonArray().get(1).getAsString();
        JsonElement funVal = expression.get(5);
        JsonElement funScope = expression.get(7);

        VarType returnType;
        VarType argType;
        try {
            returnType = Main.determineType(returnTypeStr);
            argType = Main.determineType(argTypeStr);
        } catch (Exception e) {
            return new ASTError(e.toString() + expression);
        }
        return new ASTFun(returnType, funName, argType, argName, Main.parse(funVal), Main.parse(funScope));
    }


    /**
     * Parses a function call in JSONlang
     */
    private static AST parseFunctionCall(JsonArray expression){
        String funName = expression.get(1).getAsString();
        JsonElement funArg = expression.get(2);
        return new ASTFunCall(funName, Main.parse(funArg));
    }

    /**
     * Determines the type
     */
    private static VarType determineType(String input) throws Exception {
        if (input.equals("int")) {
            return VarType.INTEGER;
        } else if (input.equals("bool")) {
            return VarType.BOOLEAN;
        } else {
            throw new Exception("Invalid Variable Assignment Type ");
        }
    }

}
