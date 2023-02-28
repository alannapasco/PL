package pl.AST;

import pl.Meaning.IMeaning;
import pl.SymbolTable.Environment;
import pl.SymbolTable.IEnvironment;
import pl.TypePrediction.Type;


public class ASTLet implements AST{
    Type varType;
    String varName;
    AST varValue;
    AST scope;

    public ASTLet(Type varType, String varName, AST varValue, AST scope){
        this.varType = varType;
        this.varName = varName;
        this.varValue = varValue;
        this.scope = scope;
    }

    @Override
    public Type typeCheck(IEnvironment<Type> env) throws Exception {
        Type typeVerified = varValue.typeCheck(env);
        if (!typeVerified.equals(varType)) {
            throw new Exception("Type Error");
        }
        return this.scope.typeCheck(new Environment<>(this.varName, this.varType, env));
    }

    @Override
    public IMeaning value(IEnvironment<IMeaning> env) throws Exception {
        IMeaning valueEvaluated = this.varValue.value(env);
        return this.scope.value(new Environment<>(this.varName, valueEvaluated, env));
    }

    @Override
    public String toString(){
        return "[let " + this.varType + " " + this.varName + " = " + this.varValue.toString() + " in " + this.scope.toString() + "]";
    }
}
