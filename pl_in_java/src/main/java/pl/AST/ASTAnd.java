package pl.AST;

import pl.Meaning.BooleanRepresentation;
import pl.Meaning.IMeaning;
import pl.SymbolTable.Accumulator;
import pl.TypePrediction.Type;

public class ASTAnd implements AST {
    final AST firstVal;
    final AST secondVal;

    public ASTAnd(AST firstVal, AST secondVal ){
        this.firstVal = firstVal;
        this.secondVal = secondVal;
    }

    @Override
    public Type typeCheck(Accumulator<Type> accumulator) throws Exception {
        if (this.firstVal.typeCheck(accumulator)== Type.BOOLEAN
                && this.secondVal.typeCheck(accumulator)== Type.BOOLEAN) {
            return Type.BOOLEAN;
        } else {
            throw new Exception("Type Error");
        }
    }

    @Override
    public IMeaning value(Accumulator<IMeaning> accumulator) throws Exception {
        if (this.firstVal.value(accumulator) instanceof BooleanRepresentation firstValRep
                && this.secondVal.value(accumulator) instanceof BooleanRepresentation secondValRep) {
            return compute(firstValRep.value, secondValRep.value);
        } else {
            throw new Exception("Invalid " + this.getClass().toString());
        }
    }

    @Override
    public AST staticDistance(String[] acc, int tailIdx) {
        return new ASTAnd(this.firstVal.staticDistance(acc, tailIdx), this.secondVal.staticDistance(acc, tailIdx));
    }

    @Override
    public int countNumLets(int count) {
        return this.firstVal.countNumLets(count) + this.secondVal.countNumLets(count);
    }

    @Override
    public IMeaning valueSD(IMeaning[] acc, int tailIdx) throws Exception {
        if (this.firstVal.valueSD(acc, tailIdx) instanceof BooleanRepresentation firstValRep
                && this.secondVal.valueSD(acc, tailIdx) instanceof BooleanRepresentation secondValRep) {
            return compute(firstValRep.value, secondValRep.value);
        } else {
            throw new Exception("Invalid " + this.getClass().toString());
        }
    }

    private IMeaning compute(boolean firstVal, boolean secondVal) {
        return new BooleanRepresentation(firstVal && secondVal);
    }

    @Override
    public String toString(){
        return "[" + this.firstVal + " ^ " + this.secondVal + "]";
    }

    @Override
    public boolean equals(Object o) {
        if (o == this) {
            return true;
        }
        if (!(o instanceof ASTAnd)) {
            return false;
        }
        ASTAnd x = (ASTAnd) o;
        return this.firstVal.equals(x.firstVal) && this.secondVal.equals(x.secondVal);
    }
}
