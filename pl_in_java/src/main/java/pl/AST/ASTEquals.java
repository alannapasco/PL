package pl.AST;

import pl.Meaning.BooleanRepresentation;
import pl.Meaning.IMeaning;
import pl.SymbolTable.IEnvironment;
import pl.TypePrediction.Type;
import pl.TypePrediction.VarType;

public class ASTEquals implements AST {
    final AST firstVal;
    final AST secondVal;

    public ASTEquals(AST firstVal, AST secondVal) {
        this.firstVal = firstVal;
        this.secondVal = secondVal;
    }

    @Override
    public Type typeCheck(IEnvironment<Type> env) throws Exception {
        //the first and second val do not need to be of the same type to call .equals() on them
        //in that case, .equals() will just return false
        return VarType.BOOLEAN;
    }

    @Override
    public IMeaning value(IEnvironment<IMeaning> env) throws Exception {
        return new BooleanRepresentation(firstVal.value(env).equals(secondVal.value(env)));
    }

    @Override
    public String toString(){
        return "[" + this.firstVal + " == " + this.secondVal + "]";
    }

    @Override
    public boolean equals(Object o) {
        if (o == this) {
            return true;
        }
        if (!(o instanceof ASTEquals x)) {
            return false;
        }
        return this.firstVal.equals(x.firstVal) && this.secondVal.equals(x.secondVal);
    }
}
