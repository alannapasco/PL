package pl.AST;

import pl.Meaning.IMeaning;
import pl.TypePrediction.TypePrediction;

public class ASTError implements AST {
    final String message;

    public ASTError(String message){
        this.message = message;
    }

    @Override
    public TypePrediction typeCheck() throws Exception {
        throw new Exception("Type Error");
    }

    @Override
    public IMeaning value() throws Exception {
        throw new Exception("Error: " + this.message);
    }

    @Override
    public String toString(){
        return "ASTERROR " + this.message;
    }
}
