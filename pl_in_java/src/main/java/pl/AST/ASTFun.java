package pl.AST;

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
        //verify that the body evaluated with the given argument returns the proper return type
        Type argTypeVerified = funBody.typeCheck(new Accumulator<>(this.argName, this.argType, accumulator));
        if (!argTypeVerified.equals(returnType)) {
            throw new Exception("Type Error - Function body does not evaluate to the correct return type");
        }

        Type funTypePair = new FunTypePair(this.argType, this.returnType);
        return this.scope.typeCheck(new Accumulator<>(this.funName, funTypePair, accumulator));
    }

    @Override
    public IMeaning value(Accumulator<IMeaning> accumulator) throws Exception {
        //get the argument
        IMeaning givenArgValue = this.scope.value(accumulator);

        //combine the argName with the givenArgValue in the accumulator
        Accumulator<IMeaning> accWithArg = new Accumulator<>(this.argName, givenArgValue, accumulator);

        //Get the value of this function's body
        return this.funBody.value(accWithArg);
    }

    @Override
    public AST staticDistance(Accumulator<Integer> accumulator) {
        return null;
    }

    @Override
    public int countNumLetsInAST(int count) {
        return count;
    }

    @Override
    public IMeaning valueSD(IMeaning[] acc, int nextFreeSlot) throws Exception {
        return null;
    }

    @Override
    public String toString(){
        return "[let fun " + this.returnType + " " + this.funName
                + " [ " + this.argType + " " + this.argName + " ] "
                + this.funBody.toString() + " in " + this.scope.toString() +"]"; }
}
