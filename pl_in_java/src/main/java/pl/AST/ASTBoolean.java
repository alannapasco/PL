package pl.AST;
import pl.Meaning.BooleanRepresentation;
import pl.Meaning.IMeaning;
import pl.TypePrediction.TypePrediction;

public class ASTBoolean implements AST {
    private final boolean value;

    public ASTBoolean(boolean value){
        this.value = value;
    }

    @Override
    public TypePrediction typeCheck() {
        return TypePrediction.BOOLEAN;
    }

    @Override
    public IMeaning value() {
        return new BooleanRepresentation(this.value);
    }

    @Override
    public String toString(){
        return String.valueOf(this.value);
    }

}
