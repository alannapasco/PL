package pl.AST;

import pl.Meaning.BooleanRepresentation;
import pl.Meaning.IMeaning;
import pl.Meaning.IntegerRepresentation;
import pl.TypePrediction.TypePrediction;

public class ASTGreaterThan implements AST {
    AST firstVal;
    AST secondVal;

    public ASTGreaterThan(AST firstVal, AST secondVal) {
        this.firstVal = firstVal;
        this.secondVal = secondVal;
    }

    @Override
    public TypePrediction typeCheck() throws Exception {
        if (this.firstVal.typeCheck()==TypePrediction.INTEGER
                && this.secondVal.typeCheck()==TypePrediction.INTEGER) {
            return TypePrediction.BOOLEAN;
        } else {
            throw new Exception("Type Error");
        }
    }

    @Override
    public IMeaning value() {
        if (this.firstVal.value() instanceof IntegerRepresentation firstValInt
                && this.secondVal.value() instanceof IntegerRepresentation secondValInt) {
            return new BooleanRepresentation(firstValInt.value > secondValInt.value);
        }
        //TODO ??
        return null;
    }

    @Override
    public String toString(){
        return "[" + this.firstVal + " > " + this.secondVal + "]";
    }
}
