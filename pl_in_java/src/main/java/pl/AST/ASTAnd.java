package pl.AST;

import pl.Meaning.BooleanRepresentation;
import pl.Meaning.IMeaning;
import pl.SymbolTable.IEnvironment;
import pl.TypePrediction.BooleanType;
import pl.TypePrediction.IntegerType;
import pl.TypePrediction.Type;

public class ASTAnd implements AST {
    final AST firstVal;
    final AST secondVal;

    public ASTAnd(AST firstVal, AST secondVal ){
        this.firstVal = firstVal;
        this.secondVal = secondVal;
    }

    @Override
    public Type typeCheck(IEnvironment<Type> env) throws Exception {
        if (this.firstVal.typeCheck(env)instanceof BooleanType
                && this.secondVal.typeCheck(env)instanceof BooleanType) {
            return new BooleanType();
        } else {
            throw new Exception("Type Error - one of the AST around an And expression is not a Boolean");
        }
    }

    @Override
    public IMeaning value(IEnvironment<IMeaning> env) throws Exception {
        if (this.firstVal.value(env) instanceof BooleanRepresentation firstValRep
                && this.secondVal.value(env) instanceof BooleanRepresentation secondValRep) {
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
        if (!(o instanceof ASTAnd x)) {
            return false;
        }
        return this.firstVal.equals(x.firstVal) && this.secondVal.equals(x.secondVal);
    }
}
