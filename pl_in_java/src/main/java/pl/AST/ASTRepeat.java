package pl.AST;

import pl.Meaning.BooleanRepresentation;
import pl.Meaning.IMeaning;
import pl.SymbolTable.IEnvironment;
import pl.TypePrediction.BooleanType;
import pl.TypePrediction.Type;

public class ASTRepeat implements AST {
    final AST expression;
    final AST stoppingCondition;

    public ASTRepeat(AST expression, AST stoppingCondition){
        this.expression = expression;
        this.stoppingCondition = stoppingCondition;
    }

    @Override
    public Type typeCheck(IEnvironment<Type> env) throws Exception {
        if (!(this.stoppingCondition.typeCheck(env) instanceof BooleanType)) {
            throw new Exception("Type Check - Stopping condition " + this.stoppingCondition + " is not a boolean");
        }
        return this.expression.typeCheck(env);
    }

    @Override
    public IMeaning value(IEnvironment<IMeaning> env) throws Exception {
        IMeaning temp;
        do {
            temp = this.expression.value(env);
        } while (!((BooleanRepresentation)this.stoppingCondition.value(env)).value);
        return temp;
    }

    @Override
    public String toString(){
        return "[Repeat " + this.expression + " Until " + this.stoppingCondition + "]";
    }

    @Override
    public boolean equals(Object o) {
        if (o == this) {
            return true;
        }
        if (!(o instanceof ASTRepeat x)) {
            return false;
        }
        return this.expression.equals(x.expression) && this.stoppingCondition.equals(x.stoppingCondition);
    }
}
