package pl;

import com.google.gson.*;
import pl.AST.AST;
import pl.Meaning.BooleanRepresentation;
import pl.Meaning.IMeaning;
import pl.Meaning.IntegerRepresentation;
import pl.SymbolTable.Accumulator;

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
            AST astWithSD = ast.staticDistance(new Accumulator<>());

            IMeaning actualVal;
            IMeaning actualValSD;

            try {
                ast.typeCheck(new Accumulator<>());
                actualVal = ast.value(new Accumulator<>());

                int numLets = ast.countNumLetsInAST(0);
                IMeaning[] valAcc = new IMeaning[numLets];
                actualValSD = astWithSD.valueSD(valAcc, numLets-1);
            } catch (Exception e) {
                //pass over invalid
                continue;
            }

            int numErrorExamples = 18;
            IMeaning expectedVal = Utils.getExpected(expOutput.get(i-numErrorExamples).getAsJsonPrimitive());
            if (expectedVal.equals(actualVal) && expectedVal.equals(actualValSD)){
                System.out.println("Test Passed: " + i);
            } else {
                System.out.println("Test Failed: " + i);
                System.out.println("Expected: " + expectedVal);
                System.out.println("Actual: " + actualVal);
                System.out.println("ActualSD: " + actualValSD);
            }
        }
    }

    private static IMeaning getExpected(JsonPrimitive expected){
        try {
            int i = expected.getAsInt();
            return new IntegerRepresentation(i);
        } catch (NumberFormatException e) {
            boolean b = expected.getAsBoolean();
            return new BooleanRepresentation(b);
        }
    }

}
