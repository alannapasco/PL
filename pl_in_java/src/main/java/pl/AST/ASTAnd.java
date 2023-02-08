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
    public AST staticDistance(Accumulator<Integer> accumulator) {
        return new ASTAnd(this.firstVal.staticDistance(accumulator), this.secondVal.staticDistance(accumulator));
    }

    @Override
    public int countNumLetsInAST(int count) {
        return this.firstVal.countNumLetsInAST(count) + this.secondVal.countNumLetsInAST(count);
    }

    @Override
    public IMeaning valueSD(IMeaning[] acc, int nextFreeSlot) throws Exception {
        if (this.firstVal.valueSD(acc, nextFreeSlot) instanceof BooleanRepresentation firstValRep
                && this.secondVal.valueSD(acc, nextFreeSlot) instanceof BooleanRepresentation secondValRep) {
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
