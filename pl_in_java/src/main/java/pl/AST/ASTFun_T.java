package pl.AST;

import pl.Meaning.IMeaning;
import pl.SymbolTable.Environment;
import pl.SymbolTable.IEnvironment;
import pl.TypePrediction.*;

/**
 * Generic Fun
 */
public class ASTFun_T implements AST {
    GenericPlaceholder genericPlaceholder;
    Type returnType;
    String funName;

    Type argType;
    String argName;

    AST funBody;
    AST scope;

    public ASTFun_T(GenericPlaceholder genericPlaceholder, Type returnType, String funName, Type argType, String argName, AST funBody, AST funScope){
        this.genericPlaceholder = genericPlaceholder;
        this.returnType = returnType;
        this.funName = funName;

        this.argType = argType;
        this.argName = argName;

        this.funBody = funBody;
        this.scope = funScope;
    }


    @Override
    public Type typeCheck(IEnvironment<Type> env) throws Exception {
        Type generic = new ArrowTypeGeneric(this.genericPlaceholder, this.argType, this.returnType);
        IEnvironment<Type> envWithGeneric = new Environment<>(this.funName, generic, env);

        IEnvironment<Type> envWithGenAndArg = new Environment<>(this.argName, this.argType, envWithGeneric);

        Type retTypeVerified = this.funBody.typeCheck(envWithGenAndArg);
        if (!retTypeVerified.equals(returnType)) {
            throw new Exception("Type Error - Function body " + this.funBody + " does not evaluate to the correct return type " + this.returnType);
        }

        return this.scope.typeCheck(envWithGeneric);
    }

    @Override
    public IMeaning value(IEnvironment<IMeaning> env) throws Exception {
        ASTFun fun = new ASTFun(this.returnType, this.funName, this.argType, this.argName, this.funBody, this.scope);
        return fun.value(env);
    }

    @Override
    public String toString(){
        return "[let tfun " + this.returnType + " " + this.funName
                + " [" + this.argType + " " + this.argName + "] "
                + this.funBody.toString() + " in " + this.scope.toString() +"]"; }


    @Override
    public boolean equals(Object o) {
        if (o == this) {
            return true;
        }
        if (!(o instanceof ASTFun x)) {
            return false;
        }
        return this.returnType.equals(x.returnType)
                && this.funName.equals(x.funName)
                && this.argType.equals(x.argType)
                && this.argName.equals(x.argName)
                && this.funBody.equals(x.funBody)
                && this.scope.equals(x.scope);
    }
}
