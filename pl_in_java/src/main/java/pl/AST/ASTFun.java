package pl.AST;

import pl.Meaning.Closure_akaFunctionEvaluationDelayed;
import pl.Meaning.IMeaning;
import pl.Meaning.IntegerRepresentation;
import pl.SymbolTable.Environment;
import pl.SymbolTable.IEnvironment;
import pl.TypePrediction.ArrowType;
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
    public Type typeCheck(IEnvironment<Type> env) throws Exception {
        Type arrow = new ArrowType(this.argType, this.returnType);
        IEnvironment<Type> environmentWithThisFunc = new Environment<>(this.funName, arrow, env);
        Type retTypeVerified = this.funBody.typeCheck(new Environment<>(this.argName, this.argType, environmentWithThisFunc));
        if (!retTypeVerified.equals(returnType)) {
            throw new Exception("Type Error - Function body " + this.funBody + " does not evaluate to the correct return type " + this.returnType);
        }

        return this.scope.typeCheck(environmentWithThisFunc);
    }

    @Override
    public IMeaning value(IEnvironment<IMeaning> env) throws Exception {
        //recursive environment
        IMeaning dummy = new IntegerRepresentation(0);
        IEnvironment<IMeaning> environmentWithPlaceholder = new Environment<>(this.funName, dummy, env);
        IMeaning closure = new Closure_akaFunctionEvaluationDelayed(this.funBody, this.argName, environmentWithPlaceholder);
        environmentWithPlaceholder.update(this.funName, closure);

        //alternate option: make the closure recursive by
        //first passing in a dummy env to the closure, then updating the closure

        return this.scope.value(new Environment<>(this.funName, closure, environmentWithPlaceholder));
    }

    @Override
    public String toString(){
        return "[let fun " + this.returnType + " " + this.funName
                + " [ " + this.argType + " " + this.argName + " ] "
                + this.funBody.toString() + " in " + this.scope.toString() +"]"; }
}
