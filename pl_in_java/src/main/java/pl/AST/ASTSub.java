package pl.AST;

import pl.Meaning.IMeaning;
import pl.Meaning.IntegerRepresentation;
import pl.SymbolTable.IEnvironment;
import pl.TypePrediction.IntegerType;
import pl.TypePrediction.Type;

public class ASTSub implements AST {
    AST firstVal;
    AST secondVal;

    public ASTSub(AST firstVal, AST secondVal) {
        this.firstVal = firstVal;
        this.secondVal = secondVal;
    }

    @Override
    public Type typeCheck(IEnvironment<Type> env) throws Exception {
        if (this.firstVal.typeCheck(env) instanceof IntegerType
                && this.secondVal.typeCheck(env) instanceof IntegerType) {
            return new IntegerType();
        } else {
            throw new Exception("Type Error - one of the AST around an Sub expression is not an Integer");
        }
    }

    @Override
    public IMeaning value(IEnvironment<IMeaning> env) throws Exception {
        if (this.firstVal.value(env) instanceof IntegerRepresentation firstValRep
                && this.secondVal.value(env) instanceof IntegerRepresentation secondValRep) {
            return compute(firstValRep.value, secondValRep.value);
        }else {
            throw new Exception("Invalid " + this.getClass().toString());
        }
    }

    private IMeaning compute(int firstVal, int secondVal) {
        return new IntegerRepresentation(firstVal - secondVal);
    }

    @Override
    public String toString(){
        return "[" + this.firstVal + " - " + this.secondVal + "]";
    }

    @Override
    public boolean equals(Object o) {
        if (o == this) {
            return true;
        }
        if (!(o instanceof ASTSub x)) {
            return false;
        }
        return this.firstVal.equals(x.firstVal) && this.secondVal.equals(x.secondVal);
    }
}
