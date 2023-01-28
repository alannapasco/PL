package pl;
import com.google.gson.*;
import pl.AST.*;

import java.util.ArrayList;

public class Main {
    public static void main(String[] args) {

        ArrayList<JsonElement> booleanExamples = Utils.processJsonExampleSeries("boolean-input.json");
        ArrayList<JsonElement> booleanExpOutput = Utils.processJsonExampleSeries("boolean-expOut.json");
        Utils.testCmp(booleanExamples, booleanExpOutput);

        ArrayList<JsonElement> integerExamples = Utils.processJsonExampleSeries("int-input.json");
        ArrayList<JsonElement> integerExpOutput = Utils.processJsonExampleSeries("int-expOut.json");
        Utils.testCmp(integerExamples, integerExpOutput);

        ArrayList<JsonElement> mixedTypeExamples = Utils.processJsonExampleSeries("mixed-type-input.json");
        ArrayList<JsonElement> mixedTypeExpOutput = Utils.processJsonExampleSeries("mixed-type-expOut.json");
        Utils.testCmp(mixedTypeExamples, mixedTypeExpOutput);

        Main.process(booleanExamples);
        Main.process(integerExamples);
        Main.process(mixedTypeExamples);

        ///////----Parse----///////
//        System.out.println();
//        Stream<AST> parsed = booleanExamples.stream().map(Main::parse);
//        parsed.forEach(System.out::println);
//        System.out.println();
//
//        parsed = integerExamples.stream().map(Main::parse);
//        parsed.forEach(System.out::println);
//        System.out.println();
//
//        parsed = mixedTypeExamples.stream().map(Main::parse);
//        parsed.forEach(System.out::println);
//        System.out.println();

    }

    /**
     * Processes (parses -> type checks -> determines the value of) the given input examples
     */
    public static void process(ArrayList<JsonElement> examples) {
        for (JsonElement example: examples) {
            AST ast = Main.parse(example);
            try {
                ast.typeCheck();
                System.out.println(ast.value());
            } catch (Exception e) {
                //Type Check failed
                System.out.println("Invalid Example");
            }
        }
    }

    /**
     * Produces an abstract syntax tree
     */
    public static AST parse(JsonElement element) {
        if (element.isJsonPrimitive()) {
            return Main.parsePrimitive(element.getAsJsonPrimitive());
        } else if (element.isJsonArray()) {
            JsonArray astList = element.getAsJsonArray();
            if (astList.size()==3) {
                JsonElement firstVal = astList.get(0);
                String op = astList.get(1).getAsString();
                JsonElement secondVal = astList.get(2);
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
                        return new ASTError("Invalid Operator: " + element + " ");
                }
            }
        }
        return new ASTError("Not Valid AST: " + element + " ");
    }

    /**
     * Parses a single node in an abstract syntax tree
     */
    private static AST parsePrimitive(JsonPrimitive element) {
        try {
            int i = element.getAsInt();
            return new ASTInteger(i);
        } catch (NumberFormatException e) {
            //here we could either have a string or boolean
            if (element.getAsBoolean() || (!element.getAsBoolean() && element.getAsString().equals("false"))) {
                return new ASTBoolean(element.getAsBoolean());
            }
        }
        return new ASTError("Invalid Primitive: " + element + " ");
    }
}