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
        throw new Exception("AST Parse Type Error " + this.message);
    }

    @Override
    public IMeaning value(IEnvironment<IMeaning> env) throws Exception {
        throw new Exception("AST Parse Error: " + this.message);
    }

    @Override
    public String toString(){
        return "***" + this.message;
    }

    @Override
    public boolean equals(Object o) {
        if (o == this) {
            return true;
        }
        if (!(o instanceof ASTError x)) {
            return false;
        }
        return this.message.equals(x.message);
    }
}
