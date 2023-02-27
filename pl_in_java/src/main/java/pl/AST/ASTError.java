package pl.AST;

import pl.Meaning.IMeaning;
import pl.SymbolTable.Accumulator;
import pl.TypePrediction.Type;


public class ASTError implements AST {
    final String message;

    public ASTError(String message){
        this.message = message;
    }

    @Override
    public Type typeCheck(Accumulator<Type> accumulator) throws Exception {
        throw new Exception("Type Error");
    }

    @Override
    public IMeaning value(Accumulator<IMeaning> accumulator) throws Exception {
        throw new Exception("Error: " + this.message);
    }

    @Override
    public String toString(){
        return "***" + this.message;
    }
}
