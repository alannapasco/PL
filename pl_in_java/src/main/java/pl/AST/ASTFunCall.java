package pl.AST;

import pl.Meaning.Closure;
import pl.Meaning.IMeaning;
import pl.SymbolTable.IEnvironment;
import pl.TypePrediction.ArrowType;
import pl.TypePrediction.Type;

public class ASTFunCall implements AST {
    AST funExpression;
    AST funArg;

    public ASTFunCall(AST funName, AST funArg){
        this.funExpression = funName;
        this.funArg = funArg;
    }

    @Override
    public Type typeCheck(IEnvironment<Type> env) throws Exception {
        Type funExpressionType = this.funExpression.typeCheck(env);
        if (funExpressionType instanceof ArrowType fn) {
            Type argType = fn.argType;
            Type retType = fn.retType;
            if (!argType.equals(this.funArg.typeCheck(env))){
                throw new Exception("Type Error - given argument type'" + argType + "' is not the correct type as specified by " + this.funExpression + "'s function signature");
            }
            return retType;
        } else {
            throw new Exception("Type Error - func name " + this.funExpression + " does not resolve to an arrow type");
        }
    }

    @Override
    public IMeaning value(IEnvironment<IMeaning> env) throws Exception {
        IMeaning funExpressionEvaluated = this.funExpression.value(env);
        if (funExpressionEvaluated instanceof Closure c){
            IMeaning argumentEvaluated = this.funArg.value(env);
            return c.execute(argumentEvaluated);
        } else {
            throw new Exception("Invalid function Call - could not resolve: " + this.funExpression);
        }
    }

    @Override
    public String toString(){
        return "funCall: " + this.funExpression + "(" + funArg + ")";
    }

    @Override
    public boolean equals(Object o) {
        if (o == this) {
            return true;
        }
        if (!(o instanceof ASTFunCall x)) {
            return false;
        }
        return this.funExpression.equals(x.funExpression) && this.funArg.equals(x.funArg);
    }
}
