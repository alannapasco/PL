package pl.AST;

import pl.Meaning.Closure_akaFunctionEvaluationDelayed;
import pl.Meaning.IMeaning;
import pl.SymbolTable.Accumulator;
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
    public Type typeCheck(Accumulator<Type> accumulator) throws Exception {
        //Collect the info needed about the function being called
        ArrowType funTypePair;
        try {
            funTypePair = (ArrowType) accumulator.get(this.funName);
        } catch (Exception e) {
            throw new Exception("Type Error - function being called has not been defined properly");
        }

        //Check that the given argument is the correct type
        if (!funTypePair.argType.equals(this.funArg.typeCheck(accumulator))){
            throw new Exception("Type Error - given argument type'" + funTypePair.argType + "' is not the correct type as specified by " + this.funName + "'s function signature");
        }
        return funTypePair.retType;
    }

    @Override
    public IMeaning value(Accumulator<IMeaning> accumulator) throws Exception {
        Closure_akaFunctionEvaluationDelayed closure = (Closure_akaFunctionEvaluationDelayed) accumulator.get(this.funName);
        IMeaning argumentEvaluated = this.funArg.value(accumulator);
        return closure.execute(argumentEvaluated);
    }

    @Override
    public String toString(){
        return "funCall: " + this.funName + "(" + funArg + ")";
    }
}
