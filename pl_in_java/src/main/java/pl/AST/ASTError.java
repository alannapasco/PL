package pl.AST;

import pl.Meaning.IMeaning;
import pl.SymbolTable.IEnvironment;
import pl.TypePrediction.Type;


public class ASTError implements AST {
    final String message;

    public ASTError(String message){
        this.message = message;
    }

    @Override
    public Type typeCheck(IEnvironment<Type> env) throws Exception {
        throw new Exception("Type Error");
    }

    @Override
    public IMeaning value(IEnvironment<IMeaning> env) throws Exception {
        throw new Exception("Error: " + this.message);
    }

    @Override
    public String toString(){
        return "***" + this.message;
    }
}
