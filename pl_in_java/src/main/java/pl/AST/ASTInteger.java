package pl.AST;

import pl.Meaning.IMeaning;
import pl.Meaning.IntegerRepresentation;
import pl.SymbolTable.TypeEntry;
import pl.SymbolTable.ValueEntry;
import pl.TypePrediction.Type;

import java.util.LinkedList;

public class ASTInteger implements AST {
    private final int value;

    public ASTInteger(int value){
        this.value = value;
    }

    @Override
    public Type typeCheck(LinkedList<TypeEntry> accumulator) {
        return Type.INTEGER;
    }

    @Override
    public IMeaning value(LinkedList<ValueEntry> accumulator) {
        return new IntegerRepresentation(this.value);
    }

    @Override
    public String toString(){
        return String.valueOf(this.value);
    }
}
