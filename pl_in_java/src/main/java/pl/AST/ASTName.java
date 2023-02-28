package pl.AST;

import pl.Meaning.IMeaning;
import pl.SymbolTable.IEnvironment;
import pl.TypePrediction.Type;


public class ASTName implements AST {
    String name;

    public ASTName(String name) {
        this.name = name;
    }

    @Override
    public Type typeCheck(IEnvironment<Type> env) throws Exception {
        try {
            return env.get(this.name);
        } catch (Exception e) {
            throw new Exception("Type Error - could not find " + this.name + " in the environment");
        }
    }

    @Override
    public IMeaning value(IEnvironment<IMeaning> env) throws Exception {
        try {
            return env.get(this.name);
        } catch (Exception e) {
            throw new Exception("Invalid " + this.getClass().toString());
        }
    }

    @Override
    public String toString(){
        return "var:" + this.name;
    }
}
