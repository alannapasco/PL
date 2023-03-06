package pl.AST;

import pl.Meaning.BooleanRepresentation;
import pl.Meaning.IMeaning;
import pl.SymbolTable.IEnvironment;
import pl.TypePrediction.Type;
import pl.TypePrediction.VarType;

public class ASTIfElse implements AST {
    final AST condition;
    final AST doExpression;
    final AST doElse;

    public ASTIfElse(AST condition, AST doExpression, AST doElse){
        this.condition = condition;
        this.doExpression = doExpression;
        this.doElse = doElse;
    }

    @Override
    public Type typeCheck(IEnvironment<Type> env) throws Exception {
        if (!this.condition.typeCheck(env).equals(VarType.BOOLEAN)) {
            throw new Exception("Type Check - if condition " + this.condition + " is not a boolean");
        }
        if (this.doExpression.typeCheck(env).equals(this.doElse.typeCheck(env))){
            return this.doExpression.typeCheck(env);
        } else {
            throw new Exception("Type Check - if expression and else branches do not return the same type");
        }
    }

    @Override
    public IMeaning value(IEnvironment<IMeaning> env) throws Exception {
        BooleanRepresentation cond = (BooleanRepresentation) this.condition.value(env);
        if (cond.value) {
            return this.doExpression.value(env);
        } else {
            return this.doElse.value(env);
        }
    }

    @Override
    public String toString(){
        return "[If " + this.condition + " Do " + this.doExpression + " Else " + this.doElse + "]";
    }

    @Override
    public boolean equals(Object o) {
        if (o == this) {
            return true;
        }
        if (!(o instanceof ASTIfElse x)) {
            return false;
        }
        return this.condition.equals(x.condition)
                && this.doExpression.equals(x.doExpression)
                && this.doElse.equals(x.doElse);
    }
}
