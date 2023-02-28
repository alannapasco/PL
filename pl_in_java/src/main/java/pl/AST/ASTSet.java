package pl.AST;

import pl.Meaning.IMeaning;
import pl.SymbolTable.IEnvironment;
import pl.TypePrediction.Type;

public class ASTSet implements AST {
    String varName;
    AST newVal;

    public ASTSet(String varName, AST newVal){
        this.varName=varName;
        this.newVal=newVal;
    }

    @Override
    public Type typeCheck(IEnvironment<Type> env) throws Exception {
        Type existingVarType = env.get(this.varName);
        Type newValType = this.newVal.typeCheck(env);
        if (!existingVarType.equals(newValType)){
            throw new Exception("Type Error");
        } else {
            return existingVarType;
        }
    }

    @Override
    public IMeaning value(IEnvironment<IMeaning> env) throws Exception {
        return env.update(this.varName, this.newVal.value(env));
    }

    @Override
    public String toString(){
        return "[set " + this.varName + " to " + this.newVal + "]";
    }
}
