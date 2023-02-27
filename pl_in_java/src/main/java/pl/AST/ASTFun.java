package pl.AST;

import pl.Meaning.Closure_akaFunctionEvaluationDelayed;
import pl.Meaning.IMeaning;
import pl.Meaning.IntegerRepresentation;
import pl.SymbolTable.Accumulator;
import pl.TypePrediction.FunTypePair;
import pl.TypePrediction.Type;

public class ASTFun implements AST {
    Type returnType;
    String funName;

    Type argType;
    String argName;

    AST funBody;
    AST scope;

    public ASTFun(Type returnType, String funName, Type argType, String argName, AST funBody, AST funScope){
        this.returnType = returnType;
        this.funName = funName;

        this.argType = argType;
        this.argName = argName;

        this.funBody = funBody;
        this.scope = funScope;
    }

    @Override
    public Type typeCheck(Accumulator<Type> accumulator) throws Exception {
        Type funTypePair = new FunTypePair(this.argType, this.returnType);
        Accumulator<Type> environmentWithThisFunc = new Accumulator<>(this.funName, funTypePair, accumulator);

        Type retTypeVerified = this.funBody.typeCheck(new Accumulator<>(this.argName, this.argType, environmentWithThisFunc));
        if (!retTypeVerified.equals(returnType)) {
            throw new Exception("Type Error - Function body " + this.funBody + " does not evaluate to the correct return type " + this.returnType);
        }

        return this.scope.typeCheck(environmentWithThisFunc);
    }

    @Override
    public IMeaning value(Accumulator<IMeaning> accumulator) throws Exception {
        //recursive environment
        IMeaning dummy = new IntegerRepresentation(0);
        Accumulator<IMeaning> environmentWithPlaceholder = new Accumulator<>(this.funName, dummy, accumulator);
        IMeaning closure = new Closure_akaFunctionEvaluationDelayed(this.funBody, this.argName, environmentWithPlaceholder);
        environmentWithPlaceholder.update(this.funName, closure);

        //alternate option: make the closure recursive by
        //first passing in a dummy env to the closure, then updating the closure 

        return this.scope.value(new Accumulator<>(this.funName, closure, environmentWithPlaceholder));
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
        return "[let fun " + this.returnType + " " + this.funName
                + " [ " + this.argType + " " + this.argName + " ] "
                + this.funBody.toString() + " in " + this.scope.toString() +"]"; }
}
