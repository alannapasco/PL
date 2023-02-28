package pl.AST;

import pl.Meaning.IMeaning;
import pl.Meaning.IntegerRepresentation;
import pl.SymbolTable.IEnvironment;
import pl.TypePrediction.Type;
import pl.TypePrediction.VarType;

public class ASTAdd implements AST {
    AST firstVal;
    AST secondVal;

    public ASTAdd(AST firstVal, AST secondVal) {
        this.firstVal = firstVal;
        this.secondVal = secondVal;
    }

    @Override
    public Type typeCheck(IEnvironment<Type> env) throws Exception {
        if (this.firstVal.typeCheck(env)== VarType.INTEGER
                && this.secondVal.typeCheck(env)== VarType.INTEGER) {
            return VarType.INTEGER;
        } else {
            throw new Exception("Type Error");
        }
    }

    @Override
    public IMeaning value(IEnvironment<IMeaning> env) throws Exception {
        if (this.firstVal.value(env) instanceof IntegerRepresentation firstValRep
                && this.secondVal.value(env) instanceof IntegerRepresentation secondValRep) {
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