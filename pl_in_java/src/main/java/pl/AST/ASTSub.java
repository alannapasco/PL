package pl.AST;

import pl.Meaning.IMeaning;
import pl.Meaning.IntegerRepresentation;
import pl.SymbolTable.Accumulator;
import pl.TypePrediction.Type;

import java.util.LinkedList;

public class ASTSub implements AST {
    AST firstVal;
    AST secondVal;

    public ASTSub(AST firstVal, AST secondVal) {
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
        if (this.firstVal.value(accumulator) instanceof IntegerRepresentation firstValInt
                && this.secondVal.value(accumulator) instanceof IntegerRepresentation secondValInt) {
            return new IntegerRepresentation(firstValInt.value - secondValInt.value);
        }else {
            throw new Exception("Invalid " + this.getClass().toString());
        }
    }

    @Override
    public String toString(){
        return "[" + this.firstVal + " - " + this.secondVal + "]";
    }
}
