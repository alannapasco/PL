package pl;

import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonParser;
import com.google.gson.JsonSyntaxException;
import pl.AST.AST;
import pl.Meaning.BooleanRepresentation;
import pl.Meaning.IMeaning;
import pl.Meaning.IntegerRepresentation;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.List;
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
        System.out.println();
        List<IMeaning> actualOut = new ArrayList<>();

        for (JsonElement example: actual) {
            AST parsed = Main.parse(example);
            try {
                parsed.typeCheck();
                actualOut.add(parsed.value());
            } catch (Exception e) {
                //pass over invalid
            }
        }

        List<IMeaning> expOut = new ArrayList<>();
        for (JsonElement solution: expOutput) {
            try {
                int s = solution.getAsInt();
                expOut.add(new IntegerRepresentation(s));
            } catch (NumberFormatException e) {
                boolean s = solution.getAsBoolean();
                expOut.add(new BooleanRepresentation(s));
            }

        }

        for (int i=0; i<expOutput.size(); i++){
            if (actualOut.get(i).equals(expOut.get(i))){
                System.out.println("Test Passed: " + i);
            } else {
                System.out.println("Test Failed: " + i);
                System.out.println("Expected: " + expOutput.get(i));
                System.out.println("Actual: " + actual.get(i));
            }
        }
    }

}