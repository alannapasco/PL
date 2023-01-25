package pl.AST;

import pl.Meaning.IMeaning;
import pl.Meaning.IntegerRepresentation;
import pl.TypePrediction.TypePrediction;

public class ASTInteger implements AST {
    private final int value;

    public ASTInteger(int value){
        this.value = value;
    }

    @Override
    public TypePrediction typeCheck() {
        return TypePrediction.INTEGER;
    }

    @Override
    public IMeaning value() {
        return new IntegerRepresentation(this.value);
    }

    @Override
    public String toString(){
        return String.valueOf(this.value);
    }
}
