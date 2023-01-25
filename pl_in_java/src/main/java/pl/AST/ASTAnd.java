package pl.AST;

import pl.Meaning.BooleanRepresentation;
import pl.Meaning.IMeaning;
import pl.TypePrediction.TypePrediction;

public class ASTAnd implements AST {
    final AST firstVal;
    final AST secondVal;

    public ASTAnd(AST firstVal, AST secondVal ){
        this.firstVal = firstVal;
        this.secondVal = secondVal;
    }

    @Override
    public TypePrediction typeCheck() throws Exception {
        if (this.firstVal.typeCheck()==TypePrediction.BOOLEAN
                && this.secondVal.typeCheck()==TypePrediction.BOOLEAN) {
            return TypePrediction.BOOLEAN;
        } else {
            throw new Exception("Type Error");
        }
    }

    @Override
    public IMeaning value() {
        if (this.firstVal.value() instanceof BooleanRepresentation
                && this.secondVal.value() instanceof BooleanRepresentation) {
            BooleanRepresentation firstValBool = (BooleanRepresentation) this.firstVal.value();
            BooleanRepresentation secondValBool = (BooleanRepresentation) this.secondVal.value();
            return new BooleanRepresentation(firstValBool.value && secondValBool.value);
        }
        //TODO ??
        return null;
    }

    @Override
    public String toString(){
        return "[" + this.firstVal + " ^ " + this.secondVal + "]";
    }
}
