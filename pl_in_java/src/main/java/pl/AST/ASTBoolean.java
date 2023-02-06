package pl.AST;
import pl.Meaning.BooleanRepresentation;
import pl.Meaning.IMeaning;
import pl.SymbolTable.Accumulator;
import pl.TypePrediction.Type;

import java.util.LinkedList;

public class ASTBoolean implements AST {
    private final boolean value;

    public ASTBoolean(boolean value){
        this.value = value;
    }

    @Override
    public Type typeCheck(Accumulator<Type> accumulator) {
        return Type.BOOLEAN;
    }

    @Override
    public IMeaning value(Accumulator<IMeaning> accumulator) {
        return new BooleanRepresentation(this.value);
    }

    @Override
    public String toString(){
        return String.valueOf(this.value);
    }

}
