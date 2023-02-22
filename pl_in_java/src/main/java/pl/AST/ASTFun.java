package pl.AST;

import pl.Meaning.Closure_akaFunctionEvaluationDelayed;
import pl.Meaning.IMeaning;
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
        //verify that the body type checked with the given argument returns the proper return type
        Type argTypeVerified = funBody.typeCheck(new Accumulator<>(this.argName, this.argType, accumulator));
        if (!argTypeVerified.equals(returnType)) {
            throw new Exception("Type Error - Function body does not evaluate to the correct return type");
        }

        Type funTypePair = new FunTypePair(this.argType, this.returnType);
        return this.scope.typeCheck(new Accumulator<>(this.funName, funTypePair, accumulator));
    }

    @Override
    public IMeaning value(Accumulator<IMeaning> accumulator) throws Exception {
        IMeaning closure = new Closure_akaFunctionEvaluationDelayed(this.funBody, this.argName, accumulator);
        return this.scope.value(new Accumulator<>(this.funName, closure, accumulator));
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
