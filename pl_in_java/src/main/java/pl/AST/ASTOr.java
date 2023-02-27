package pl.AST;

import pl.Meaning.BooleanRepresentation;
import pl.Meaning.IMeaning;
import pl.SymbolTable.Accumulator;
import pl.TypePrediction.Type;
import pl.TypePrediction.VarType;


public class ASTOr implements AST {
    final AST firstVal;
    final AST secondVal;

    public ASTOr(AST firstVal, AST secondVal ){
        this.firstVal = firstVal;
        this.secondVal = secondVal;
    }

    @Override
    public Type typeCheck(Accumulator<Type> accumulator) throws Exception {
        if (this.firstVal.typeCheck(accumulator)== VarType.BOOLEAN
                && this.secondVal.typeCheck(accumulator)== VarType.BOOLEAN) {
            return VarType.BOOLEAN;
        } else {
            throw new Exception("Type Error");
        }
    }

    @Override
    public IMeaning value(Accumulator<IMeaning> accumulator) throws Exception {
        if (this.firstVal.value(accumulator) instanceof BooleanRepresentation firstValRep
                && this.secondVal.value(accumulator) instanceof BooleanRepresentation secondValRep) {
            return compute(firstValRep.value, secondValRep.value);
        }else {
            throw new Exception("Invalid " + this.getClass().toString());
        }
    }

    private IMeaning compute(boolean firstVal, boolean secondVal) {
        return new BooleanRepresentation(firstVal || secondVal);
    }

    @Override
    public String toString(){
        return "[" + this.firstVal + " || " + this.secondVal + "]";
    }

    @Override
    public boolean equals(Object o) {
        if (o == this) {
            return true;
        }
        if (!(o instanceof ASTOr)) {
            return false;
        }
        ASTOr x = (ASTOr) o;
        return this.firstVal.equals(x.firstVal) && this.secondVal.equals(x.secondVal);
    }

}
