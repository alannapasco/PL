package pl.AST;

import pl.Meaning.BooleanRepresentation;
import pl.Meaning.IMeaning;
import pl.SymbolTable.TypeEntry;
import pl.SymbolTable.ValueEntry;
import pl.TypePrediction.Type;

import java.util.LinkedList;

public class ASTAnd implements AST {
    final AST firstVal;
    final AST secondVal;

    public ASTAnd(AST firstVal, AST secondVal ){
        this.firstVal = firstVal;
        this.secondVal = secondVal;
    }

    @Override
    public Type typeCheck(LinkedList<TypeEntry> accumulator) throws Exception {
        if (this.firstVal.typeCheck(accumulator)== Type.BOOLEAN
                && this.secondVal.typeCheck(accumulator)== Type.BOOLEAN) {
            return Type.BOOLEAN;
        } else {
            throw new Exception("Type Error");
        }
    }

    @Override
    public IMeaning value(LinkedList<ValueEntry> accumulator) throws Exception {
        if (this.firstVal.value(accumulator) instanceof BooleanRepresentation firstValBool
                && this.secondVal.value(accumulator) instanceof BooleanRepresentation secondValBool) {
            return new BooleanRepresentation(firstValBool.value && secondValBool.value);
        } else {
            throw new Exception("Invalid " + this.getClass().toString());
        }
    }

    @Override
    public String toString(){
        return "[" + this.firstVal + " ^ " + this.secondVal + "]";
    }
}
