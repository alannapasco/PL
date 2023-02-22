package pl.AST;

import pl.Meaning.Closure_akaFunctionEvaluationDelayed;
import pl.Meaning.IMeaning;
import pl.SymbolTable.Accumulator;
import pl.TypePrediction.FunTypePair;
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
        FunTypePair funTypePair;
        try {
            funTypePair = (FunTypePair) accumulator.get(this.funName);
        } catch (Exception e) {
            throw new Exception("Type Error - function being called has not been defined properly");
        }

        //Check that the given argument is the correct type
        if (!funTypePair.argType.equals(this.funArg.typeCheck(accumulator))){
            throw new Exception("Type Error - given argument is not the correct type as specified by the function signature");
        }
        return funTypePair.retType;
    }

    @Override
    public IMeaning value(Accumulator<IMeaning> accumulator) throws Exception {
        //fetch the closure containing all the required information
      // MF: this cast is one of those that should never fail ... but again,
      // Java's type system cannot convey this information from the TC pass to the value pas
        Closure_akaFunctionEvaluationDelayed closure = (Closure_akaFunctionEvaluationDelayed) accumulator.get(this.funName);

        //evaluate the value of the given argument
        IMeaning argumentEvaluated = this.funArg.value(accumulator);

	// MF: in the spirit of "introduction" and "elimination", the last line should be in the Closure_akaFun... object
	// it's called `apply` in traditional terminology .. and you can now see where the "command pattern" comes from
        //evaluate the function body using the given value of the function argument
        return closure.apply(argumentEvaluated);
    }

    @Override
    public AST staticDistance(Accumulator<Integer> accumulator) {
      // MF: throw an `RuntimeException` instead, the ones that are _not_ declared and type-checked.
        return null;
    }

    @Override
    public int countNumLetsInAST(int count) {
        return count;
    }

    @Override
    public IMeaning valueSD(IMeaning[] acc, int nextFreeSlot) throws Exception {
      // MF: throw an `RuntimeException` instead, the ones that are _not_ declared and type-checked.
        return null;
    }

    @Override
    public String toString(){
        return "funCall: " + this.funName + "(" + funArg + ")";
    }
}
