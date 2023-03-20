package pl.AST;

import pl.Meaning.IMeaning;
import pl.SymbolTable.IEnvironment;
import pl.TypePrediction.Type;

public class ASTFunT implements AST {
    Type returnType;
    String funName;

    Type argType;
    String argName;

    AST funBody;
    AST scope;

    public ASTFunT(Type returnType, String funName, Type argType, String argName, AST funBody, AST funScope){
        this.returnType = returnType;
        this.funName = funName;

        this.argType = argType;
        this.argName = argName;

        this.funBody = funBody;
        this.scope = funScope;
    }


    @Override
    public Type typeCheck(IEnvironment<Type> env) throws Exception {
        return null;
    }

    @Override
    public IMeaning value(IEnvironment<IMeaning> env) throws Exception {
        return null;
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
