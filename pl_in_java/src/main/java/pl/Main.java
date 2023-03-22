package pl;
import com.google.gson.*;
import pl.AST.*;
import pl.SymbolTable.MtEnvironment;
import pl.TypePrediction.*;

import java.util.ArrayList;

/**
[ StackOverflow example:
 "let", "fun", "int", "f", ["int", "x"], "x", "in",
 ["let", "fun", "int", "g", ["int", "y"], ["call", "f", "y"], "in",
 ["let", "var", ["int", "->", "int"], "_", "=", ["set", "f", "g"], "in",
 ["call", "f", 0]]]
 ]
 */

public class Main {
    public static void main(String[] args) {

        //names are types in Java (nominal typing) - has become dominant typing through java/C
        //the alternative is structural typing
        //when you override .equals, you are creating structural typing
        //typescript uses structural typing
        //structural  is more expensive than nominal
        //people thought nominal typing was 'better' for the human brain while programming
        //structural typing solves:
        //type systems restrict what you can say -- the reason programs terminate is because of the type systems
        // -- if you remove the types then you can write infinite loops in a few weeks of developing a PL
        //the current type system makes you copy code -- nominal typing makes you copy code -- structural does no
        //
        //the distinction is that the TYPE CHECKER uses nominal vs structural comparison -- but both
        //languages (TS and java) ALLOW for both.
        // == is nominal
        // .equals() is structural

        ArrayList<JsonElement> input = Utils.processJsonExampleSeries("input.json");
        ArrayList<JsonElement> output = Utils.processJsonExampleSeries("output.json");

        Utils.testCmp(input, output);
        Main.process(input);

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
                ast.typeCheck(new MtEnvironment<>());
                System.out.println("Value: " + ast.value(new MtEnvironment<>()));
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
            return Main.parseSingleElement(element.getAsJsonPrimitive());
        } else if (element.isJsonArray()) {
            return Main.parseArray(element.getAsJsonArray());
        }
        return new ASTError("Not Valid AST: " + element + " ");
    }


    /**
     * Parses a single element (non-array) of JSONLang
     */
    private static AST parseSingleElement(JsonPrimitive element) {
        try {
            int i = element.getAsInt();
            return new ASTInteger(i);
        } catch (NumberFormatException e) {
            if (element.getAsBoolean() || element.getAsString().equals("false")) {
                return new ASTBoolean(element.getAsBoolean());
            } else {
                return new ASTName(element.getAsString());
            }
        }
    }


    /**
     * Parses an element of JSONLang that is represented by a JSON array
     */
    private static AST parseArray(JsonArray expression) {
        String action = "";
        try {
            action = expression.get(0).getAsString();
        } catch (IllegalStateException e) {
            //do not throw an error here; continue program flow
        }

        if (action.equals("let")) {
            String what = expression.get(1).getAsString();
            if (what.equals("var")) {
                return Main.parseVariableDeclaration(expression);
            } else if (what.equals("fun")) {
                return Main.parseFunction(expression);
            } else if (what.equals("tfun")) {
                return Main.parseTFunction(expression);
            }
        } else if (action.equals("call")) {
            return Main.parseFunctionCall(expression);
        } else if (action.equals("tcall")) {
            return Main.parseTFunctionCall(expression);
        } else if (action.equals("set")) {
            return Main.parseVariableAssignment(expression);
        } else if (action.equals("repeat")) {
            return Main.parseRepeat(expression);
        } else if (action.equals("if")) {
            return Main.parseIfExpression(expression);
        } else if (expression.size()==3) {
            return Main.parseOperation(expression);
        }
        return new ASTError("Invalid Expression: " + expression + " ");
    }


    /**
     * Parses a variable declaration in JSONLang
     */
    private static AST parseVariableDeclaration(JsonArray expression){
        JsonElement varTypeInput = expression.get(2);
        String varName = expression.get(3).getAsString();
        JsonElement varVal = expression.get(5);
        JsonElement varScope = expression.get(7);

        Type varType;
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
        JsonElement returnType = expression.get(2);
        String funName = expression.get(3).getAsString();
        JsonElement argType = expression.get(4).getAsJsonArray().get(0);
        String argName = expression.get(4).getAsJsonArray().get(1).getAsString();
        JsonElement funVal = expression.get(5);
        JsonElement funScope = expression.get(7);

        Type returnTypeDetermined;
        Type argTypeDetermined;
        try {
            returnTypeDetermined = Main.determineType(returnType);
            argTypeDetermined = Main.determineType(argType);
        } catch (Exception e) {
            return new ASTError(e.toString() + expression);
        }
        return new ASTFun(returnTypeDetermined, funName, argTypeDetermined, argName, Main.parse(funVal), Main.parse(funScope));
    }

    /**
     * Parses a function<T> in JSONlang
     */
    private static AST parseTFunction(JsonArray expression){
        JsonElement genericPlaceholder = expression.get(2);
        JsonElement returnType = expression.get(3);
        String funName = expression.get(4).getAsString();
        JsonElement argType = expression.get(5).getAsJsonArray().get(0);
        String argName = expression.get(5).getAsJsonArray().get(1).getAsString();
        JsonElement funVal = expression.get(6);
        JsonElement funScope = expression.get(8);

        GenericPlaceholder genericPlaceholderDetermined;
        Type returnTypeDetermined;
        Type argTypeDetermined;
        try {
            genericPlaceholderDetermined = (GenericPlaceholder) Main.determineTType(genericPlaceholder);
            returnTypeDetermined = Main.determineTType(returnType);
            argTypeDetermined = Main.determineTType(argType);
        } catch (Exception e) {
            return new ASTError(e.toString() + expression);
        }
        return new ASTFun_T(genericPlaceholderDetermined, returnTypeDetermined, funName, argTypeDetermined, argName, Main.parse(funVal), Main.parse(funScope));
    }


    /**
     * Parses a function call in JSONlang
     */
    private static AST parseFunctionCall(JsonArray expression){
        JsonElement funName = expression.get(1);
        JsonElement funArg = expression.get(2);
        return new ASTFunCall(Main.parse(funName), Main.parse(funArg));
    }

    /**
     * Parses a function<T> call in JSONlang
     */
    private static AST parseTFunctionCall(JsonArray expression){
        JsonElement funName = expression.get(1);
        JsonElement genericActual = expression.get(2);

        Type genericTypeDetermined;
        try {
            genericTypeDetermined = Main.determineTType(genericActual);
        } catch (Exception e) {
            return new ASTError(e.toString() + expression);
        }

        return new ASTFunCall_T(Main.parse(funName), genericTypeDetermined);
    }

    /**
     * Parses a variable (re)assignment in JSONlang
     */
    private static AST parseVariableAssignment(JsonArray expression){
        String varName = expression.get(1).getAsString();
        JsonElement newVal = expression.get(2);
        return new ASTSet(varName, parse(newVal));
    }


    /**
     * Parses a repeat _ until _ expression in JSONlang
     */
    private static AST parseRepeat(JsonArray expression){
        JsonElement theDo = expression.get(1);
        JsonElement until = expression.get(3);
        return new ASTRepeat(parse(theDo), parse(until));
    }


    /**
     * Parses an if expression in JSONlang
     */
    private static AST parseIfExpression(JsonArray expression) {
        JsonElement condition = expression.get(1);
        JsonElement doExpression = expression.get(2);
        JsonElement doElse = expression.get(4);
        return new ASTIfElse(parse(condition), parse(doExpression), parse(doElse));
    }


    /**
     * Parses an operation in JSONLang
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
            case "==":
                return new ASTEquals(parse(firstVal), parse(secondVal));
            default:
                return new ASTError("Invalid Operator in expression: " + operationExpression + " ");
        }
    }


    /**
     * Determines the type of variable being declared or assigned in JSONlang
     */
    private static Type determineType(JsonElement input) {
        try {
            String typeAsString = input.getAsString();
            if (typeAsString.equals("int")) {
                return new IntegerType();
            } else if (typeAsString.equals("bool")) {
                return new BooleanType();
            } else {
                throw new Exception("Invalid Variable Assignment Type");
            }
        } catch (Exception e){
            JsonArray arrowType = input.getAsJsonArray();
            Type declared = determineType(arrowType.get(0));
            Type returned = determineType(arrowType.get(2));
            return new ArrowType(declared, returned);
        }
    }

    /**
     * Determines the type of variable being declared or assigned in JSONlang
     * expression which includes generics
     */
    private static Type determineTType(JsonElement input) {
        try {
            String typeAsString = input.getAsString();
            if (typeAsString.equals("int")) {
                return new IntegerType();
            } else if (typeAsString.equals("bool")) {
                return new BooleanType();
            } else {
                return new GenericPlaceholder(typeAsString);
            }
        } catch (Exception e){
            JsonArray arrowType = input.getAsJsonArray();
            Type declared = determineType(arrowType.get(0));
            Type returned = determineType(arrowType.get(2));
            return new ArrowType(declared, returned);
        }
    }
}
