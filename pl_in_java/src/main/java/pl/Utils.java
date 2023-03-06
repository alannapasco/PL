package pl;

import com.google.gson.*;
import pl.AST.AST;
import pl.Meaning.BooleanRepresentation;
import pl.Meaning.Closure;
import pl.Meaning.IMeaning;
import pl.Meaning.IntegerRepresentation;
import pl.SymbolTable.MtEnvironment;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.Scanner;

public class Utils {

    /**Processes a series of test examples in `filename`, returns ArrayList of test examples */
    public static ArrayList<JsonElement> processJsonExampleSeries(String filename) {
        try {
            File inputFile = new File(filename);
            Scanner scanner = new Scanner(inputFile);
            scanner.useDelimiter("\\Z");
            String input = scanner.next();
            JsonElement inputJsonElement = new JsonParser().parse(input);
            JsonArray inputJsonArray = inputJsonElement.getAsJsonArray();
            return new ArrayList<>(inputJsonArray.asList());
        } catch (JsonSyntaxException | FileNotFoundException | NullPointerException e) {
            e.printStackTrace();
            return new ArrayList<>();
        }
    }

    /**Processes a single test example in `filename`, returns the single test example */
    public static JsonElement processJsonSingleExample(String filename){
        try {
            File inputFile = new File(filename);
            Scanner scanner = new Scanner(inputFile);
            scanner.useDelimiter("\\Z");
            String input = scanner.next();
            JsonElement jsonElement = new JsonParser().parse(input);
            return jsonElement;
        } catch (JsonSyntaxException | FileNotFoundException e) {
            e.printStackTrace();
            return null;
        }
    }

    /**helps compare input/output for automated testing */
    public static void testCmp(ArrayList<JsonElement> actual, ArrayList<JsonElement> expOutput){
        for (int i=0; i<actual.size(); i++){

            AST ast = Main.parse(actual.get(i));
            IMeaning actualVal;

            try {
                ast.typeCheck(new MtEnvironment<>());
                actualVal = ast.value(new MtEnvironment<>());
            } catch (Exception e) {
                System.out.println("TestCmp Failed at tc(): " + i);
                continue;
            }

            int numErrorExamples = 22;
            IMeaning expectedVal = Utils.getExpected(expOutput.get(i-numErrorExamples).getAsJsonPrimitive());
            if (expectedVal.equals(actualVal)){
                System.out.println("Test Passed: " + i);
            } else {
                System.out.println("Test Failed: " + i);
                System.out.println("Expected: " + expectedVal);
                System.out.println("Actual: " + actualVal);
            }
        }
        System.out.println();
    }

    private static IMeaning getExpected(JsonPrimitive expected){
        try {
            int i = expected.getAsInt();
            return new IntegerRepresentation(i);
        } catch (NumberFormatException e) {
            if (expected.getAsBoolean() || expected.getAsString().equals("false")) {
                boolean b = expected.getAsBoolean();
                return new BooleanRepresentation(b);
            } else {
                return new Closure();
            }


        }
    }

}
