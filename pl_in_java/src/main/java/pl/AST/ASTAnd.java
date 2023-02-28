package pl.AST;

import pl.Meaning.BooleanRepresentation;
import pl.Meaning.IMeaning;
import pl.SymbolTable.IEnvironment;
import pl.TypePrediction.Type;
import pl.TypePrediction.VarType;

public class ASTAnd implements AST {
    final AST firstVal;
    final AST secondVal;

    public ASTAnd(AST firstVal, AST secondVal ){
        this.firstVal = firstVal;
        this.secondVal = secondVal;
    }

    @Override
    public Type typeCheck(IEnvironment<Type> env) throws Exception {
        if (this.firstVal.typeCheck(env)== VarType.BOOLEAN
                && this.secondVal.typeCheck(env)== VarType.BOOLEAN) {
            return VarType.BOOLEAN;
        } else {
            throw new Exception("Type Error");
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
        if (!(o instanceof ASTAnd)) {
            return false;
        }
        ASTAnd x = (ASTAnd) o;
        return this.firstVal.equals(x.firstVal) && this.secondVal.equals(x.secondVal);
    }
}
