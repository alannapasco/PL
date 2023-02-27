package pl.AST;

import pl.Meaning.IMeaning;
import pl.SymbolTable.Accumulator;
import pl.TypePrediction.Type;


public class ASTName implements AST {
    String name;

    public ASTName(String name) {
        this.name = name;
    }

    @Override
    public Type typeCheck(Accumulator<Type> accumulator) throws Exception {
        try {
            return accumulator.get(this.name);
        } catch (Exception e) {
            throw new Exception("Type Error - could not find " + this.name + " in the environment");
        }
    }

    @Override
    public IMeaning value(Accumulator<IMeaning> accumulator) throws Exception {
        try {
            return accumulator.get(this.name);
        } catch (Exception e) {
            throw new Exception("Invalid " + this.getClass().toString());
        }
    }

    @Override
    public String toString(){
        return "var:" + this.name;
    }
}
