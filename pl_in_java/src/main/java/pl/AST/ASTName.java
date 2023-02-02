package pl.AST;

import pl.Meaning.IMeaning;
import pl.SymbolTable.TypeEntry;
import pl.SymbolTable.ValueEntry;
import pl.TypePrediction.Type;

import java.util.LinkedList;

public class ASTName implements AST {
    String name;

    public ASTName(String name) {
        this.name = name;
    }

    @Override
    public Type typeCheck(LinkedList<TypeEntry> accumulator) throws Exception {
        for (TypeEntry e : accumulator) {
            if (e.name().equals(this.name)){
                return e.type();
            }
        }
        throw new Exception("Type Error");
    }

    @Override
    public IMeaning value(LinkedList<ValueEntry> accumulator) throws Exception {
        for (ValueEntry e : accumulator) {
            if (e.name().equals(this.name)){
                return e.value();
            }
        }
        throw new Exception("Invalid " + this.getClass().toString());
    }

    @Override
    public String toString(){
        return "name:" + this.name;
    }
}
