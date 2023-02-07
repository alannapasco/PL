package pl.AST;

import pl.Meaning.BooleanRepresentation;
import pl.Meaning.IMeaning;
import pl.Meaning.IntegerRepresentation;
import pl.SymbolTable.Accumulator;
import pl.TypePrediction.Type;

public class ASTGreaterThan implements AST {
    AST firstVal;
    AST secondVal;

    public ASTGreaterThan(AST firstVal, AST secondVal) {
        this.firstVal = firstVal;
        this.secondVal = secondVal;
    }

    @Override
    public Type typeCheck(Accumulator<Type> accumulator) throws Exception {
        if (this.firstVal.typeCheck(accumulator)== Type.INTEGER
                && this.secondVal.typeCheck(accumulator)== Type.INTEGER) {
            return Type.BOOLEAN;
        } else {
            throw new Exception("Type Error");
        }
    }

    @Override
    public IMeaning value(Accumulator<IMeaning> accumulator) throws Exception {
        if (this.firstVal.value(accumulator) instanceof IntegerRepresentation firstValRep
                && this.secondVal.value(accumulator) instanceof IntegerRepresentation secondValRep) {
            return compute(firstValRep.value, secondValRep.value);
        }else {
            throw new Exception("Invalid " + this.getClass().toString());
        }
    }

    @Override
    public AST staticDistance(String[] acc, int tailIdx) {
        return new ASTGreaterThan(this.firstVal.staticDistance(acc, tailIdx), this.secondVal.staticDistance(acc, tailIdx));
    }

    @Override
    public int countNumLets(int count) {
        return this.firstVal.countNumLets(count) + this.secondVal.countNumLets(count);
    }

    @Override
    public IMeaning valueSD(IMeaning[] acc, int tailIdx) throws Exception {
        if (this.firstVal.valueSD(acc, tailIdx) instanceof IntegerRepresentation firstValRep
                && this.secondVal.valueSD(acc, tailIdx) instanceof IntegerRepresentation secondValRep) {
            return compute(firstValRep.value, secondValRep.value);
        } else {
            throw new Exception("Invalid " + this.getClass().toString());
        }
    }

    private IMeaning compute(int firstVal, int secondVal) {
        return new BooleanRepresentation(firstVal > secondVal);
    }

    @Override
    public String toString(){
        return "[" + this.firstVal + " > " + this.secondVal + "]";
    }
}
