package pl.AST;

import pl.Meaning.IMeaning;
import pl.Meaning.IntegerRepresentation;
import pl.SymbolTable.Accumulator;
import pl.TypePrediction.Type;

public class ASTAdd implements AST {
    AST firstVal;
    AST secondVal;

    public ASTAdd(AST firstVal, AST secondVal) {
        this.firstVal = firstVal;
        this.secondVal = secondVal;
    }

    @Override
    public Type typeCheck(Accumulator<Type> accumulator) throws Exception {
        if (this.firstVal.typeCheck(accumulator)== Type.INTEGER
                && this.secondVal.typeCheck(accumulator)== Type.INTEGER) {
            return Type.INTEGER;
        } else {
            throw new Exception("Type Error");
        }
    }

    @Override
    public IMeaning value(Accumulator<IMeaning> accumulator) throws Exception {
        if (this.firstVal.value(accumulator) instanceof IntegerRepresentation firstValRep
                && this.secondVal.value(accumulator) instanceof IntegerRepresentation secondValRep) {
            return compute(firstValRep.value, secondValRep.value);
        } else {
            throw new Exception("Invalid " + this.getClass().toString());
        }
    }

    @Override
    public AST staticDistance(Accumulator<Integer> accumulator) {
        return new ASTAdd(this.firstVal.staticDistance(accumulator), this.secondVal.staticDistance(accumulator));
    }

    @Override
    public int countNumLetsInAST(int count) {
        return this.firstVal.countNumLetsInAST(count) + this.secondVal.countNumLetsInAST(count);
    }

    @Override
    public IMeaning valueSD(IMeaning[] acc, int nextFreeSlot) throws Exception {
        if (this.firstVal.valueSD(acc, nextFreeSlot) instanceof IntegerRepresentation firstValRep
                && this.secondVal.valueSD(acc, nextFreeSlot) instanceof IntegerRepresentation secondValRep) {
            return compute(firstValRep.value, secondValRep.value);
        } else {
            throw new Exception("Invalid " + this.getClass().toString());
        }
    }

    private IMeaning compute(int firstVal, int secondVal) {
        return new IntegerRepresentation(firstVal + secondVal);
    }

    @Override
    public String toString(){
        return "[" + this.firstVal + " + " + this.secondVal + "]";
    }
}