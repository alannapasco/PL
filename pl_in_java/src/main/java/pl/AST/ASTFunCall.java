package pl.AST;

import pl.Meaning.Closure_akaFunctionEvaluationDelayed;
import pl.Meaning.IMeaning;
import pl.SymbolTable.IEnvironment;
import pl.TypePrediction.ArrowType;
import pl.TypePrediction.Type;

public class ASTFunCall implements AST {
    String funName;
    AST funArg;

    public ASTFunCall(String funName, AST funArg){
        this.funName = funName;
        this.funArg = funArg;
    }

    @Override
    public Type typeCheck(IEnvironment<Type> env) throws Exception {
        //Collect the info needed about the function being called
        ArrowType arrow;
        try {
            arrow = (ArrowType) env.get(this.funName);
        } catch (Exception e) {
            throw new Exception("Type Error - function being called has not been defined properly");
        }

        //Check that the given argument is the correct type
        if (!arrow.argType.equals(this.funArg.typeCheck(env))){
            throw new Exception("Type Error - given argument type'" + arrow.argType + "' is not the correct type as specified by " + this.funName + "'s function signature");
        }
        return arrow.retType;
    }

    @Override
    public IMeaning value(IEnvironment<IMeaning> env) throws Exception {
        Closure_akaFunctionEvaluationDelayed closure = (Closure_akaFunctionEvaluationDelayed) env.get(this.funName);
        IMeaning argumentEvaluated = this.funArg.value(env);
        return closure.execute(argumentEvaluated);
    }

    @Override
    public String toString(){
        return "funCall: " + this.funName + "(" + funArg + ")";
    }
}
