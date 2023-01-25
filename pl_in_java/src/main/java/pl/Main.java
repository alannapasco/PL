package pl;
import com.google.gson.*;
import pl.AST.*;
import pl.TypePrediction.TypePrediction;

import java.util.ArrayList;
import java.util.stream.Stream;

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


        ///////----Parse----///////
        System.out.println();
        Stream<AST> parsed = booleanExamples.stream().map(Main::parse);
        parsed.forEach(System.out::println);
        System.out.println();

        parsed = integerExamples.stream().map(Main::parse);
        parsed.forEach(System.out::println);
        System.out.println();

        parsed = mixedTypeExamples.stream().map(Main::parse);
        parsed.forEach(System.out::println);
        System.out.println();
    }

    public static AST parse(JsonElement element) {
        if (element.isJsonPrimitive()) {
            try {
                int i = element.getAsInt();
                return new ASTInteger(i);
            } catch (NumberFormatException e) {
                //here we could either have a string or boolean
            }
            if (element.getAsBoolean() || (!element.getAsBoolean() && element.getAsString().equals("false"))) {
                return new ASTBoolean(element.getAsBoolean());
            }
            new ASTError("Invalid Primitive: " + element + " ");
        } else if (element.isJsonArray()) {
            JsonArray astList = element.getAsJsonArray();
            if (astList.size()==3) {
                JsonElement firstVal = astList.get(0);
                String op = astList.get(1).getAsString();
                JsonElement secondVal = astList.get(2);
                switch (op) {
                    case "^":
                        try {
                            TypePrediction tpFirst = parse(firstVal).typeCheck();
                            TypePrediction tpSecond = parse(secondVal).typeCheck();
                            if (tpFirst==TypePrediction.BOOLEAN && tpSecond==TypePrediction.BOOLEAN) {
                                return new ASTAnd(parse(firstVal), parse(secondVal));
                            }
                        } catch (Exception e) {
                            return new ASTError("Invalid Expression: " + element + " ");
                        }
                    case "||":
                        try {
                            TypePrediction tpFirst = parse(firstVal).typeCheck();
                            TypePrediction tpSecond = parse(secondVal).typeCheck();
                            if (tpFirst==TypePrediction.BOOLEAN && tpSecond==TypePrediction.BOOLEAN) {
                                return new ASTOr(parse(firstVal), parse(secondVal));
                            }
                        } catch (Exception e) {
                            return new ASTError("Invalid Expression: " + element + " ");
                        }
                    case ">":
                        try {
                            TypePrediction tpFirst = parse(firstVal).typeCheck();
                            TypePrediction tpSecond = parse(secondVal).typeCheck();
                            if (tpFirst==TypePrediction.INTEGER && tpSecond==TypePrediction.INTEGER) {
                                return new ASTGreaterThan(parse(firstVal), parse(secondVal));
                            }
                        } catch (Exception e) {
                            return new ASTError("Invalid Expression: " + element + " ");
                        }
                    case "-":
                        try {
                            TypePrediction tpFirst = parse(firstVal).typeCheck();
                            TypePrediction tpSecond = parse(secondVal).typeCheck();
                            if (tpFirst==TypePrediction.INTEGER && tpSecond==TypePrediction.INTEGER) {
                                return new ASTSub(parse(firstVal), parse(secondVal));
                            }
                        } catch (Exception e) {
                            return new ASTError("Invalid Expression: " + element + " ");
                        }
                    case "+":
                        try {
                            TypePrediction tpFirst = parse(firstVal).typeCheck();
                            TypePrediction tpSecond = parse(secondVal).typeCheck();
                            if (tpFirst==TypePrediction.INTEGER && tpSecond==TypePrediction.INTEGER) {
                                return new ASTAdd(parse(firstVal), parse(secondVal));
                            }
                        } catch (Exception e) {
                            return new ASTError("Invalid Expression: " + element + " ");
                        }
                    default:
                        return new ASTError("Invalid Operator: " + element + " ");
                }
            }
        }
        return new ASTError("Not Valid AST: " + element + " ");
    }

    private static boolean validOperator(JsonElement operator){
        String op = operator.getAsString();
        return op.equals("^") || op.equals("||") || op.equals(">") || op.equals("-") || op.equals("+");
    }
}